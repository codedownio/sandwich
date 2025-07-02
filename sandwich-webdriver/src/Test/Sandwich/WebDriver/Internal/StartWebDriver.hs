{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sandwich.WebDriver.Internal.StartWebDriver (
  startWebDriver
  , stopWebDriver

  , DriverType(..)
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader (MonadReader)
import Control.Retry
import Data.Default
import Data.Function (fix)
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO (BufferMode(..), hClose, hGetLine, hSetBuffering)
import Test.Sandwich
import Test.Sandwich.Contexts.Util.Ports (findFreePortOrException)
import Test.Sandwich.Util.Process
import Test.Sandwich.WebDriver.Internal.Capabilities.Extra
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Timeout

#ifndef mingw32_HOST_OS
import Test.Sandwich.WebDriver.Internal.StartWebDriver.Xvfb
#endif


type Constraints m = (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadMask m, MonadFail m
  )

data DriverType =
  DriverTypeSeleniumJar FilePath FilePath
  | DriverTypeGeckodriver FilePath
  | DriverTypeChromedriver FilePath

-- | Spin up a WebDriver process and return a 'WebDriver'
startWebDriver :: (
  Constraints m, MonadReader context m, HasBaseContext context, HasBrowserDependencies context
  ) => DriverType -> WdOptions -> OnDemandOptions -> FilePath -> m WebDriverContext
startWebDriver driverType wdOptions@(WdOptions {capabilities=capabilities'', ..}) (OnDemandOptions {..}) runRoot = do
  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> liftIO makeUUID

  -- Directory to log everything for this webdriver
  let webdriverRoot = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True webdriverRoot

  -- Directory to hold any downloads
  let downloadDir = webdriverRoot </> "Downloads"
  liftIO $ createDirectoryIfMissing True downloadDir

  let (programName, mkArgs) = case driverType of
        DriverTypeSeleniumJar java seleniumPath -> (java, \port -> do
          (driverArgs, capabilities') <- fillInCapabilitiesAndGetDriverArgs webdriverRoot capabilities''
          let allArgs = driverArgs <> ["-jar", seleniumPath, "-port", show port]
          return (allArgs, capabilities')
          )
        DriverTypeGeckodriver geckodriver -> (geckodriver, \port -> do
          (_driverArgs :: [String], capabilities') <- fillInCapabilitiesAndGetDriverArgs webdriverRoot capabilities''
          return (["--port", show port], capabilities')
          )
        DriverTypeChromedriver chromedriver -> (chromedriver, \port -> do
          (_driverArgs :: [String], capabilities') <- fillInCapabilitiesAndGetDriverArgs webdriverRoot capabilities''
          return (["--port=" <> show port], capabilities')
          )

  -- Set up xvfb if configured
  xvfbOnDemand <- newMVar OnDemandNotStarted
  (maybeXvfbSession, maybeEnv) <- case runMode of
#ifndef mingw32_HOST_OS
    RunInXvfb (XvfbConfig {..}) -> do
      (s, e) <- makeXvfbSession xvfbResolution xvfbStartFluxbox webdriverRoot xvfbToUse xvfbOnDemand
      return (Just s, Just e)
#endif
    _ -> return (Nothing, Nothing)

  -- Create a distinct process name
  webdriverProcessName <- ("webdriver_process_" <>) <$> (liftIO makeUUID)
  let webdriverProcessRoot = webdriverRoot </> T.unpack webdriverProcessName
  liftIO $ createDirectoryIfMissing True webdriverProcessRoot

  -- Retry up to 10 times
  -- This is necessary because sometimes we get a race for the port we get from findFreePortOrException.
  -- There doesn't seem to be any way to make Selenium choose its own port.
  let policy = constantDelay 0 <> limitRetries 10
  (port, hRead, p, capabilities') <- recoverAll policy $ \retryStatus -> flip withException (\(e :: SomeException) -> warn [i|Exception in startWebDriver retry: #{e}|]) $ do
    when (rsIterNumber retryStatus > 0) $
      warn [i|Trying again to start selenium server (attempt #{rsIterNumber retryStatus})|]

    (hRead, hWrite) <- createPipe
    port <- findFreePortOrException

    liftIO $ hSetBuffering hRead LineBuffering
    -- It's possible we don't need to set this on hWrite, but only on hRead
    liftIO $ hSetBuffering hWrite LineBuffering

    (args, capabilities') <- mkArgs port

    let cp = (proc programName args) {
               env = maybeEnv
               , std_in = Inherit
               , std_out = UseHandle hWrite
               , std_err = UseHandle hWrite
               , create_group = True
             }

    -- Start the process and wait for it to be ready
    debug [i|#{programName} #{T.unwords $ fmap T.pack args}|]

    (_, _, _, p) <- liftIO $ createProcess cp

    let teardown = do
          gracefullyStopProcess p 30_000_000
          liftIO $ hClose hRead

    -- On exception, make sure the process is gone and the pipe handle is closed
    flip withException (\(_ :: SomeException) -> teardown) $ do
      let needle = case driverType of
            DriverTypeSeleniumJar {} -> "Selenium Server is up and running"
            DriverTypeChromedriver {} -> "ChromeDriver was started successfully"
            DriverTypeGeckodriver {} -> "Listening on"

      -- Read from the (combined) output stream until we see the up and running message,
      -- or the process ends and we get an exception from hGetLine
      startupResult <- timeout 10_000_000 $ fix $ \loop -> do
        line <- fmap T.pack $ liftIO $ hGetLine hRead
        debug line

        if | needle `T.isInfixOf` line -> return ()
           | otherwise -> loop

      case startupResult of
        Nothing -> do
          let msg = [i|Didn't see "#{needle}" line in output after 10s.|]
          warn msg
          expectationFailure (T.unpack msg)
        Just () -> return (port, hRead, p, capabilities')

  logAsync <- async $ forever $ do
    T.pack <$> liftIO (hGetLine hRead) >>= debug

  -- Final extra capabilities configuration
  capabilities <-
    pure capabilities'
    >>= configureChromeNoSandbox wdOptions
    -- >>= configureChromeUserDataDir
    >>= configureHeadlessChromeCapabilities wdOptions runMode
    >>= configureHeadlessFirefoxCapabilities wdOptions runMode
    >>= configureChromeDownloadCapabilities downloadDir
    >>= configureFirefoxDownloadCapabilities downloadDir

  WebDriverContext
    <$> pure (T.unpack webdriverName)
    <*> pure (p, maybeXvfbSession)
    <*> pure (wdOptions {
               capabilities = capabilities
             })
    <*> liftIO (newMVar mempty)
    <*> pure (def { W._wdPort = fromIntegral port
                  , W._wdCapabilities = capabilities
                  , W._wdHTTPManager = httpManager
                  , W._wdHTTPRetryCount = httpRetryCount
                  , W._wdBasePath = case driverType of
                      DriverTypeSeleniumJar {} -> "/wd/hub"
                      _ -> ""
                  })
    <*> pure downloadDir

    <*> pure ffmpegToUse
    <*> newMVar OnDemandNotStarted

    <*> pure xvfbToUse
    <*> pure xvfbOnDemand

    <*> pure logAsync


stopWebDriver :: Constraints m => WebDriverContext -> m ()
stopWebDriver (WebDriverContext {wdWebDriver=(p, maybeXvfbSession), wdLogAsync}) = do
  -- | TODO: expose this as an option
  let gracePeriod :: Int
      gracePeriod = 30_000_000

  gracefullyStopProcess p gracePeriod

  whenJust maybeXvfbSession $ \(XvfbSession {..}) -> do
    whenJust xvfbFluxboxProcess $ \p' -> do
      gracefullyStopProcess p' gracePeriod

    gracefullyStopProcess xvfbProcess gracePeriod

  cancel wdLogAsync
