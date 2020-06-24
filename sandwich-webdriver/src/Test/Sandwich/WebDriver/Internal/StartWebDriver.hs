{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, TypeOperators, LambdaCase, DataKinds, UndecidableInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}
-- |

module Test.Sandwich.WebDriver.Internal.StartWebDriver where


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Retry
import qualified Data.Aeson as A
import Data.Default
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (PortNumber)
import Safe
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Ports
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Monad as W
import qualified Test.WebDriver.Session as W

#ifdef linux_HOST_OS
import System.Posix.IO
import System.Posix.Types
#endif


-- | Spin up a Selenium WebDriver and create a WdSession
startWebDriver :: WdOptions -> FilePath -> IO WdSession
startWebDriver wdOptions@(WdOptions {capabilities=capabilities', ..}) runRoot = do
  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> makeUUID

  -- Set up config
  port <- findFreePortOrException
  let capabilities = case runMode of
        RunHeadless -> capabilities' { W.browser = browser'}
          where browser' = case W.browser capabilities of
                  x@(W.Chrome {..}) -> x { W.chromeOptions = "--headless":chromeOptions }
                  x -> error [i|Headless mode not yet supported for browser '#{x}'|]
        _ -> capabilities'
  let wdConfig = (def { W.wdPort = fromIntegral port, W.wdCapabilities = capabilities })

  -- Get the CreateProcess
  createDirectoryIfMissing True toolsRoot
  (wdCreateProcess, maybeXvfbSession) <- getWebdriverCreateProcess wdOptions runRoot port >>= \case
    Left err -> error [i|Failed to create webdriver process: '#{err}'|]
    Right x -> return x

  -- Open output handles
  let logsDir = runRoot </> (T.unpack webdriverName)
  createDirectoryIfMissing True logsDir
  hout <- openFile (logsDir </> seleniumOutFileName) AppendMode
  herr <- openFile (logsDir </> seleniumErrFileName) AppendMode

  -- Start the process and wait for it to be ready
  (_, _, _, p) <- createProcess $ wdCreateProcess {
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
    T.readFile (logsDir </> seleniumErrFileName) >>= \case
      t | readyMessage `T.isInfixOf` t -> return True
      _ -> T.readFile (logsDir </> seleniumOutFileName) >>= \case
        t | readyMessage `T.isInfixOf` t -> return True
        _ -> return False
  unless success $ do
    interruptProcessGroupOf p >> waitForProcess p
    error [i|Selenium server failed to start after 60 seconds|]

  -- Make the WdSession
  WdSession <$> pure (T.unpack webdriverName)
            <*> pure (hout, herr, p, logsDir </> seleniumOutFileName, logsDir </> seleniumErrFileName, maybeXvfbSession)
            <*> pure wdOptions
            <*> newMVar mempty
            <*> newMVar 0
            <*> newMVar mempty
            <*> pure wdConfig


stopWebDriver :: WdSession -> IO ()
stopWebDriver (WdSession {wdWebDriver=(hout, herr, h, _, _, maybeXvfbSession)}) = do
  interruptProcessGroupOf h >> waitForProcess h
  hClose hout
  hClose herr

  whenJust maybeXvfbSession $ \(XvfbSession {..}) -> do
    interruptProcessGroupOf xvfbProcess >> waitForProcess xvfbProcess

-- * Util

seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"

getWebdriverCreateProcess :: WdOptions -> FilePath -> PortNumber -> IO (Either T.Text (CreateProcess, Maybe XvfbSession))
getWebdriverCreateProcess (WdOptions {toolsRoot, runMode}) runRoot port = runExceptT $ do
  chromeDriverPath <- ExceptT $ downloadChromeDriverIfNecessary toolsRoot
  seleniumPath <- ExceptT $ downloadSeleniumIfNecessary toolsRoot

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

      tmpDir <- liftIO getCanonicalTemporaryDirectory
      (path, tmpHandle) <- liftIO $ openTempFile tmpDir "x11_server_num"
      Fd fd <- liftIO $ handleToFd tmpHandle

      serverNum <- liftIO findFreeServerNum

      -- Start the Xvfb session
      let authFile = runRoot </> ".Xauthority"
      liftIO $ createDirectoryIfMissing True runRoot
      liftIO $ writeFile authFile ""
      (_, _, _, p) <- liftIO $ createProcess $ (proc "Xvfb" [":" <> show serverNum
                                                            , "-screen", "0", [i|#{w}x#{h}x24|]
                                                            , "-displayfd", [i|#{fd}|]
                                                            , "-auth", authFile
                                                            ]) { cwd = Just runRoot
                                                               , create_group = True }

      -- When a displayfd filepath is provided, try to obtain the X11 screen
      xvfbSession@(XvfbSession {..}) <- liftIO $ do
        let retryPolicy = constantDelay 10000 <> limitRetries 1000
        recoverAll retryPolicy $ \_ ->
          readFile path >>= \contents -> case readMay contents of
            Nothing -> throwIO $ userError [i|Couldn't determine X11 screen to use. Got data: '#{contents}'. File was '#{path}'|]
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
