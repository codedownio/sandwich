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
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
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
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Ports
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W

#ifdef linux_HOST_OS
import System.Posix.IO as Posix
import System.Posix.Types
#endif

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
  let webDriverRoot = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True webDriverRoot

  -- Get the CreateProcess
  debug [i|Preparing to create the Selenium process|]
  liftIO $ createDirectoryIfMissing True toolsRoot
  (wdCreateProcess, maybeXvfbSession) <- getWebdriverCreateProcess wdOptions runRoot port >>= \case
    Left err -> error [i|Failed to create webdriver process: '#{err}'|]
    Right x -> return x

  -- Open output handles
  let seleniumOutPath = webDriverRoot </> seleniumOutFileName
  hout <- liftIO $ openFile seleniumOutPath AppendMode
  let seleniumErrPath = webDriverRoot </> seleniumErrFileName
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

-- * Util

seleniumOutFileName, seleniumErrFileName :: FilePath
seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"

getWebdriverCreateProcess :: Constraints m => WdOptions -> FilePath -> PortNumber -> m (Either T.Text (CreateProcess, Maybe XvfbSession))
getWebdriverCreateProcess (WdOptions {toolsRoot, runMode, seleniumToUse, chromeDriverToUse}) webdriverRoot port = runExceptT $ do
  seleniumPath <- ExceptT $ obtainSelenium toolsRoot seleniumToUse
  chromeDriverPath <- ExceptT $ obtainChromeDriver toolsRoot chromeDriverToUse

  (javaEnv, xvfbSession) <- case runMode of
    RunInXvfb (XvfbConfig {xvfbResolution, ..}) -> do
      let (w, h) = fromMaybe (1920, 1080) xvfbResolution
      liftIO $ createDirectoryIfMissing True webdriverRoot

      -- TODO: forgo temporary file and just use createPipeFd
      -- (readFd, writeFd) <- liftIO Posix.createPipe
      -- readHandle <- liftIO $ fdToHandle readFd

      xvfbOut <- liftIO $ openFile (webdriverRoot </> "xvfb_out.log") AppendMode
      xvfbErr <- liftIO $ openFile (webdriverRoot </> "xvfb_err.log") AppendMode

      let policy = constantDelay 10000 <> limitRetries 1000
      (serverNum :: Int, p, authFile, displayNum) <- lift $ recoverAll policy $ \_ -> do
        withTempFile webdriverRoot "x11_server_num" $ \path tmpHandle -> do
          fd <- liftIO $ handleToFd tmpHandle
          (serverNum, p, authFile) <- createXvfbSession webdriverRoot w h fd xvfbOut xvfbErr -- writeFd

          debug [i|Trying to determine display number for auth file '#{authFile}', using '#{path}'|]

          displayNum <- liftIO $
            readFile path >>= \contents -> case readMay contents of -- hGetContents readHandle
              Nothing -> throwIO $ userError [i|Couldn't determine X11 screen to use. Got data: '#{contents}'. Path was '#{path}'|]
              Just (x :: Int) -> return x

          return (serverNum, p, authFile, displayNum)

      fluxboxProcess <- if xvfbStartFluxbox then Just <$> (startFluxBoxOnDisplay webdriverRoot displayNum) else return Nothing

      let xvfbSession = XvfbSession {
            xvfbDisplayNum = displayNum
            , xvfbXauthority = authFile
            , xvfbDimensions = (w, h)
            , xvfbProcess = p
            , xvfbOut = xvfbOut
            , xvfbErr = xvfbErr
            , xvfbFluxboxProcess = fluxboxProcess
            }

      -- TODO: allow verbose logging to be controlled with an option:
      env' <- liftIO getEnvironment
      let env = L.nubBy (\x y -> fst x == fst y) $ [("DISPLAY", ":" <> show serverNum)
                                                   , ("XAUTHORITY", xvfbXauthority xvfbSession)] <> env'
      return (Just env, Just xvfbSession)
    _ -> return (Nothing, Nothing)

  return ((proc "java" [[i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                       , [i|-Dwebdriver.chrome.logfile=#{webdriverRoot </> "chromedriver.log"}|]
                       , [i|-Dwebdriver.chrome.verboseLogging=true|]
                       , "-jar", seleniumPath
                       , "-port", show port]) { env = javaEnv }, xvfbSession)


createXvfbSession :: Constraints m => FilePath -> Int -> Int -> Fd -> Handle -> Handle -> m (Int, ProcessHandle, FilePath)
createXvfbSession webdriverRoot w h (Fd fd) xvfbOut xvfbErr = do
  serverNum <- liftIO findFreeServerNum

  -- Start the Xvfb session
  authFile <- liftIO $ writeTempFile webdriverRoot ".Xauthority" ""
  (_, _, _, p) <- liftIO $ createProcess $ (proc "Xvfb" [":" <> show serverNum
                                                        , "-screen", "0", [i|#{w}x#{h}x24|]
                                                        , "-displayfd", [i|#{fd}|]
                                                        , "-auth", authFile
                                                        ]) { cwd = Just webdriverRoot
                                                           , create_group = True
                                                           , std_out = UseHandle xvfbOut
                                                           , std_err = UseHandle xvfbErr }

  return (serverNum, p, authFile)


-- * Util

findFreeServerNum :: IO Int
findFreeServerNum = findFreeServerNum' 99
  where
    findFreeServerNum' :: Int -> IO Int
    findFreeServerNum' candidate = do
      doesPathExist [i|/tmp/.X11-unix/X#{candidate}|] >>= \case
        True -> findFreeServerNum' (candidate + 1)
        False -> return candidate


startFluxBoxOnDisplay :: Constraints m => FilePath -> Int -> m ProcessHandle
startFluxBoxOnDisplay webdriverRoot x = do
  logPath <- liftIO $ writeTempFile webdriverRoot "fluxbox.log" ""

  debug [i|Starting fluxbox on logPath '#{logPath}'|]

  let args = ["-display", ":" <> show x
             , "-log", logPath]

  (_, _, _, p) <- liftIO $ createProcess $ (proc "fluxbox" args) {
    cwd = Just webdriverRoot
    , create_group = True
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

  -- TODO: confirm fluxbox started successfully

  return p

configureBrowser browser@(W.Chrome {..}) (RunHeadless (HeadlessConfig {..})) =
  browser { W.chromeOptions = "--headless":resolution:chromeOptions }
  where resolution = [i|--window-size=#{w},#{h}|]
        (w, h) = fromMaybe (1920, 1080) headlessResolution
configureBrowser browser (RunHeadless {}) = error [i|Headless mode not yet supported for browser '#{browser}'|]
configureBrowser browser _ = browser
