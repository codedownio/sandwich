{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, TypeOperators, LambdaCase, DataKinds, UndecidableInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}
-- |

module Test.Sandwich.WebDriver.Internal.StartWebDriver where


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except
import Control.Retry
import Data.Default
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import Network.Socket (PortNumber)
import Safe
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Ports
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W

#ifdef linux_HOST_OS
import System.Posix.IO as Posix
import System.Posix.Types
#endif

type Constraints m = (HasCallStack, MonadLogger m, MonadIO m, MonadBaseControl IO m)


-- | Spin up a Selenium WebDriver and create a WdSession
startWebDriver :: Constraints m => WdOptions -> FilePath -> m WdSession
startWebDriver wdOptions@(WdOptions {capabilities=capabilities', ..}) runRoot = do
  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> (liftIO makeUUID)

  -- Set up config
  port <- liftIO findFreePortOrException
  let capabilities = case runMode of
        RunHeadless -> capabilities' { W.browser = browser'}
          where browser' = case W.browser capabilities of
                  x@(W.Chrome {..}) -> x { W.chromeOptions = "--headless":chromeOptions }
                  x -> error [i|Headless mode not yet supported for browser '#{x}'|]
        _ -> capabilities'
  let wdConfig = (def { W.wdPort = fromIntegral port, W.wdCapabilities = capabilities })

  -- Get the CreateProcess
  liftIO $ createDirectoryIfMissing True toolsRoot
  (wdCreateProcess, maybeXvfbSession) <- getWebdriverCreateProcess wdOptions runRoot port >>= \case
    Left err -> error [i|Failed to create webdriver process: '#{err}'|]
    Right x -> return x

  -- Open output handles
  let logsDir = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True logsDir
  hout <- liftIO $ openFile (logsDir </> seleniumOutFileName) AppendMode
  herr <- liftIO $ openFile (logsDir </> seleniumErrFileName) AppendMode

  -- Start the process and wait for it to be ready
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
  let retryPolicy = constantDelay 60000 <> limitRetries 1000
  success <- retrying retryPolicy (\_retryStatus result -> return (not result)) $ const $
    (liftIO $ T.readFile (logsDir </> seleniumErrFileName)) >>= \case
      t | readyMessage `T.isInfixOf` t -> return True
      _ -> (liftIO $ T.readFile (logsDir </> seleniumOutFileName)) >>= \case
        t | readyMessage `T.isInfixOf` t -> return True
        _ -> return False
  unless success $ liftIO $ do
    interruptProcessGroupOf p >> waitForProcess p
    error [i|Selenium server failed to start after 60 seconds|]

  -- Make the WdSession
  WdSession <$> pure (T.unpack webdriverName)
            <*> pure (hout, herr, p, logsDir </> seleniumOutFileName, logsDir </> seleniumErrFileName, maybeXvfbSession)
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
    liftIO (interruptProcessGroupOf xvfbProcess >> waitForProcess xvfbProcess)

-- * Util

seleniumOutFileName, seleniumErrFileName :: FilePath
seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"

getWebdriverCreateProcess :: Constraints m => WdOptions -> FilePath -> PortNumber -> m (Either T.Text (CreateProcess, Maybe XvfbSession))
getWebdriverCreateProcess (WdOptions {toolsRoot, runMode, seleniumToUse, chromeDriverToUse}) runRoot port = runExceptT $ do
  seleniumPath <- ExceptT $ obtainSelenium toolsRoot seleniumToUse
  chromeDriverPath <- ExceptT $ obtainChromeDriver toolsRoot chromeDriverToUse

  case runMode of
    Normal -> return ((proc "java" [
                         [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                         , "-jar", seleniumPath
                         , "-port", show port
                         ]) { cwd = Just runRoot }
                         , Nothing)

    RunHeadless ->
      -- Headless mode is controlled in the capabilities
      return ((proc "java" [
                 [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                 , "-jar", seleniumPath
                 , "-port", show port
                 ]) { cwd = Just runRoot }
                 , Nothing)

#ifdef linux_HOST_OS
    RunInXvfb (XvfbConfig {xvfbResolution}) -> do
      let (w, h) = fromMaybe (1920, 1080) xvfbResolution
      liftIO $ createDirectoryIfMissing True runRoot

      -- TODO: forgo temporary file and just use createPipeFd
      -- (readFd, writeFd) <- liftIO Posix.createPipe
      -- readHandle <- liftIO $ fdToHandle readFd

      tmpDir <- liftIO getCanonicalTemporaryDirectory
      (path, tmpHandle) <- liftIO $ openTempFile tmpDir "x11_server_num"
      Fd fd <- liftIO $ handleToFd tmpHandle

      let policy = constantDelay 10000 <> limitRetries 1000

      (serverNum, p) <- liftIO $ recoverAll policy $ \_ -> createXvfbSession runRoot w h fd -- writeFd

      -- When a displayfd filepath is provided, try to obtain the X11 screen
      xvfbSession@(XvfbSession {..}) <- liftIO $ do
        recoverAll policy $ \_ ->
          readFile path >>= \contents -> case readMay contents of -- hGetContents readHandle
            Nothing -> throwIO $ userError [i|Couldn't determine X11 screen to use. Got data: '#{contents}'. Path was '#{path}'|]
            Just x -> return $ XvfbSession { xvfbDisplayNum = x
                                           , xvfbXauthority = runRoot </> ".Xauthority"
                                           , xvfbDimensions = (w, h)
                                           , xvfbProcess = p }

      -- TODO: allow verbose logging to be controlled with an option:
      env' <- liftIO getEnvironment
      let env = L.nubBy (\x y -> fst x == fst y) $ [("DISPLAY", ":" <> show serverNum)
                                                   , ("XAUTHORITY", xvfbXauthority)] <> env'
      return ((proc "java" [[i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                           , [i|-Dwebdriver.chrome.logfile=#{runRoot </> "chromedriver.log"}|]
                           , [i|-Dwebdriver.chrome.verboseLogging=true|]
                           , "-jar", seleniumPath
                           , "-port", show port]) { env = Just env }, Just xvfbSession)


createXvfbSession runRoot w h fd = do
  serverNum <- liftIO findFreeServerNum

  -- Start the Xvfb session
  authFile <- writeTempFile runRoot ".Xauthority" ""
  (_, _, _, p) <- liftIO $ createProcess $ (proc "Xvfb" [":" <> show serverNum
                                                        , "-screen", "0", [i|#{w}x#{h}x24|]
                                                        , "-displayfd", [i|#{fd}|]
                                                        , "-auth", authFile
                                                        ]) { cwd = Just runRoot
                                                           , create_group = True
                                                           , std_out = CreatePipe
                                                           , std_err = CreatePipe }

  return (serverNum, p)


#else
    RunInXvfb (XvfbConfig { xvfbResolution }) -> error [i|RunInXvfb can only be used on Linux.|]
#endif



-- * Util

findFreeServerNum :: IO Int
findFreeServerNum = findFreeServerNum' 99
  where
    findFreeServerNum' :: Int -> IO Int
    findFreeServerNum' candidate = do
      doesPathExist [i|/tmp/.X11-unix/X#{candidate}|] >>= \case
        True -> findFreeServerNum' (candidate + 1)
        False -> return candidate
