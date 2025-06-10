{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sandwich.WebDriver.Internal.StartWebDriver where

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
import System.IO (hClose, hGetLine)
import Test.Sandwich
import Test.Sandwich.Contexts.Files
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

-- | Spin up a Selenium WebDriver and create a WebDriver
startWebDriver :: (
  Constraints m, MonadReader context m, HasBaseContext context
  , HasFile context "java", HasFile context "selenium.jar", HasBrowserDependencies context
  ) => WdOptions -> OnDemandOptions -> FilePath -> m WebDriver
startWebDriver wdOptions@(WdOptions {capabilities=capabilities'', ..}) (OnDemandOptions {..}) runRoot = do
  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> liftIO makeUUID

  -- Directory to log everything for this webdriver
  let webdriverRoot = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True webdriverRoot

  -- Directory to hold any downloads
  let downloadDir = webdriverRoot </> "Downloads"
  liftIO $ createDirectoryIfMissing True downloadDir

  -- Get selenium, driver args, and capabilities with browser paths applied
  java <- askFile @"java"
  seleniumPath <- askFile @"selenium.jar"
  (driverArgs, capabilities') <- fillInCapabilitiesAndGetDriverArgs webdriverRoot capabilities''

  -- Set up xvfb if configured
  xvfbOnDemand <- newMVar OnDemandNotStarted
  (maybeXvfbSession, javaEnv) <- case runMode of
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
  (port, hRead, p) <- recoverAll policy $ \retryStatus -> flip withException (\(e :: SomeException) -> warn [i|Exception in startWebDriver retry: #{e}|]) $ do
    when (rsIterNumber retryStatus > 0) $
      warn [i|Trying again to start selenium server (attempt #{rsIterNumber retryStatus})|]

    (hRead, hWrite) <- createPipe
    port <- findFreePortOrException

    let allArgs = driverArgs <> ["-jar", seleniumPath
                                , "-port", show port]
    let cp = (proc java allArgs) {
               env = javaEnv
               , std_in = Inherit
               , std_out = UseHandle hWrite
               , std_err = UseHandle hWrite
               , create_group = True
             }

    -- Start the process and wait for it to be ready
    debug [i|#{java} #{T.unwords $ fmap T.pack allArgs}|]

    (_, _, _, p) <- liftIO $ createProcess cp

    let teardown = do
          gracefullyStopProcess p 30_000_000
          liftIO $ hClose hRead

    -- On exception, make sure the process is gone and the pipe handle is closed
    flip withException (\(_ :: SomeException) -> teardown) $ do
      -- Read from the (combined) output stream until we see the up and running message,
      -- or the process ends and we get an exception from hGetLine
      startupResult <- timeout 10_000_000 $ fix $ \loop -> do
        line <- fmap T.pack $ liftIO $ hGetLine hRead
        debug line

        if | "Selenium Server is up and running" `T.isInfixOf` line -> return ()
           | otherwise -> loop

      case startupResult of
        Nothing -> do
          let msg = [i|Didn't see "up and running" line in Selenium output after 10s.|]
          warn msg
          expectationFailure (T.unpack msg)
        Just () -> return (port, hRead, p)

  -- TODO: save this in the WebDriver to tear it down later?
  _logAsync <- async $ forever (liftIO (hGetLine hRead) >>= (debug . T.pack))

  -- Final extra capabilities configuration
  capabilities <-
    pure capabilities'
    >>= configureChromeNoSandbox wdOptions
    >>= configureChromeUserDataDir
    >>= configureHeadlessCapabilities wdOptions runMode
    >>= configureDownloadCapabilities downloadDir

  -- Make the WebDriver
  WebDriver <$> pure (T.unpack webdriverName)
            <*> pure (p, maybeXvfbSession)
            <*> pure (wdOptions {
                       capabilities = capabilities
                     })
            <*> liftIO (newMVar mempty)
            <*> pure (def { W.wdPort = fromIntegral port
                          , W.wdCapabilities = capabilities
                          , W.wdHTTPManager = httpManager
                          , W.wdHTTPRetryCount = httpRetryCount
                          })
            <*> pure downloadDir

            <*> pure ffmpegToUse
            <*> newMVar OnDemandNotStarted

            <*> pure xvfbToUse
            <*> pure xvfbOnDemand


stopWebDriver :: Constraints m => WebDriver -> m ()
stopWebDriver (WebDriver {wdWebDriver=(h, maybeXvfbSession)}) = do
  -- | TODO: expose this as an option
  let gracePeriod :: Int
      gracePeriod = 30000000

  gracefullyStopProcess h gracePeriod

  whenJust maybeXvfbSession $ \(XvfbSession {..}) -> do
    whenJust xvfbFluxboxProcess $ \p -> do
      gracefullyStopProcess p gracePeriod

    gracefullyStopProcess xvfbProcess gracePeriod
