{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Test.Sandwich.WebDriver.Internal.StartWebDriver where


import Control.Concurrent
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry
import Data.Default
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Ports
import Test.Sandwich.WebDriver.Internal.StartWebDriver.Xvfb
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W

type Constraints m = (HasCallStack, MonadLogger m, MonadIO m, MonadBaseControl IO m, MonadMask m)

-- | Spin up a Selenium WebDriver and create a WdSession
startWebDriver :: Constraints m => WdOptions -> FilePath -> m WdSession
startWebDriver wdOptions@(WdOptions {capabilities=capabilities', ..}) runRoot = do
  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> (liftIO makeUUID)

  -- Set up config
  port <- liftIO findFreePortOrException
  let capabilities = capabilities' { W.browser = configureBrowser (W.browser capabilities') runMode }
  let wdConfig = (def { W.wdPort = fromIntegral port, W.wdCapabilities = capabilities })

  -- Directory to long everything for this webdriver
  let webdriverRoot = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True webdriverRoot

  -- Get the CreateProcess
  debug [i|Preparing to create the Selenium process|]
  liftIO $ createDirectoryIfMissing True toolsRoot

  -- Get selenium and chromedriver
  seleniumPath <- obtainSelenium toolsRoot seleniumToUse >>= \case
    Left err -> error [i|Failed to obtain selenium: '#{err}'|]
    Right p -> return p
  chromeDriverPath <- obtainChromeDriver toolsRoot chromeDriverToUse >>= \case
    Left err -> error [i|Failed to obtain chromedriver: '#{err}'|]
    Right p -> return p
  (maybeXvfbSession, javaEnv) <- case runMode of
    RunInXvfb (XvfbConfig {..}) -> do
      (s, e) <- getXvfbSession xvfbResolution xvfbStartFluxbox webdriverRoot
      return (Just s, Just e)
    _ -> return (Nothing, Nothing)
  let wdCreateProcess = (proc "java" [[i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                                     , [i|-Dwebdriver.chrome.logfile=#{webdriverRoot </> "chromedriver.log"}|]
                                     , [i|-Dwebdriver.chrome.verboseLogging=true|]
                                     , "-jar", seleniumPath
                                     , "-port", show port]) { env = javaEnv }

  -- Open output handles
  let seleniumOutPath = webdriverRoot </> seleniumOutFileName
  hout <- liftIO $ openFile seleniumOutPath AppendMode
  let seleniumErrPath = webdriverRoot </> seleniumErrFileName
  herr <- liftIO $ openFile seleniumErrPath AppendMode

  -- Start the process and wait for it to be ready
  debug [i|Starting the Selenium process|]
  (_, _, _, p) <- liftIO $ createProcess $ wdCreateProcess {
    std_in = Inherit
    , std_out = UseHandle hout
    , std_err = UseHandle herr
    , create_group = True
    }
  -- Normally Selenium prints the ready message to stderr. However, when we're running under
  -- XVFB the two streams get combined and sent to stdout; see
  -- https://bugs.launchpad.net/ubuntu/+source/xorg-server/+bug/1059947
  -- As a result, we poll both files
  let readyMessage = "Selenium Server is up and running"
  -- Retry every 60ms, for up to 60s before admitting defeat
  let policy = constantDelay 60000 <> limitRetries 1000
  success <- retrying policy (\_retryStatus result -> return (not result)) $ const $
    (liftIO $ T.readFile seleniumErrPath) >>= \case
      t | readyMessage `T.isInfixOf` t -> return True
      _ -> (liftIO $ T.readFile seleniumOutPath) >>= \case
        t | readyMessage `T.isInfixOf` t -> return True
        _ -> return False
  unless success $ liftIO $ do
    interruptProcessGroupOf p >> waitForProcess p
    error [i|Selenium server failed to start after 60 seconds|]

  -- Make the WdSession
  WdSession <$> pure (T.unpack webdriverName)
            <*> pure (hout, herr, p, seleniumOutPath, seleniumErrPath, maybeXvfbSession)
            <*> pure wdOptions
            <*> liftIO (newMVar mempty)
            <*> liftIO (newMVar 0)
            <*> liftIO (newMVar mempty)
            <*> pure wdConfig


stopWebDriver :: Constraints m => WdSession -> m ()
stopWebDriver (WdSession {wdWebDriver=(hout, herr, h, _, _, maybeXvfbSession)}) = do
  liftIO (interruptProcessGroupOf h >> waitForProcess h)
  liftIO $ hClose hout
  liftIO $ hClose herr

  whenJust maybeXvfbSession $ \(XvfbSession {..}) -> do
    whenJust xvfbFluxboxProcess $ \p ->
      liftIO (interruptProcessGroupOf p >> waitForProcess p)

    liftIO (interruptProcessGroupOf xvfbProcess >> waitForProcess xvfbProcess)

    liftIO $ hClose xvfbOut
    liftIO $ hClose xvfbErr

-- * Util

seleniumOutFileName, seleniumErrFileName :: FilePath
seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"



-- * Util

configureBrowser browser@(W.Chrome {..}) (RunHeadless (HeadlessConfig {..})) =
  browser { W.chromeOptions = "--headless":resolution:chromeOptions }
  where resolution = [i|--window-size=#{w},#{h}|]
        (w, h) = fromMaybe (1920, 1080) headlessResolution
configureBrowser browser (RunHeadless {}) = error [i|Headless mode not yet supported for browser '#{browser}'|]
configureBrowser browser _ = browser
