{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.WebDriver.Internal.StartWebDriver where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry
import qualified Data.Aeson as A
import Data.Default
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import GHC.Stack
import Lens.Micro
import Lens.Micro.Aeson
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Binaries.Util (detectChromeVersion)
import Test.Sandwich.WebDriver.Internal.Ports
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Firefox.Profile as FF

#ifndef mingw32_HOST_OS
import Test.Sandwich.WebDriver.Internal.StartWebDriver.Xvfb
#endif

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
fromText :: T.Text -> A.Key
fromText = A.fromText
#else
import qualified Data.HashMap.Strict        as HM
fromText :: T.Text -> T.Text
fromText = id
#endif


type Constraints m = (HasCallStack, MonadLogger m, MonadIO m, MonadBaseControl IO m, MonadMask m)

-- | Spin up a Selenium WebDriver and create a WebDriver
startWebDriver :: Constraints m => WdOptions -> FilePath -> m WebDriver
startWebDriver wdOptions@(WdOptions {..}) runRoot = do
  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> liftIO makeUUID

  -- Directory to log everything for this webdriver
  let webdriverRoot = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True webdriverRoot

  let downloadDir = webdriverRoot </> "Downloads"
  liftIO $ createDirectoryIfMissing True downloadDir

  -- Get selenium and chromedriver
  debug [i|Preparing to create the Selenium process|]
  liftIO $ createDirectoryIfMissing True toolsRoot
  seleniumPath <- obtainSelenium toolsRoot seleniumToUse >>= \case
    Left err -> error [i|Failed to obtain selenium: '#{err}'|]
    Right p -> return p
  driverArgs <- case W.browser capabilities of
    W.Firefox {} -> do
      obtainGeckoDriver toolsRoot geckoDriverToUse >>= \case
        Left err -> error [i|Failed to obtain geckodriver: '#{err}'|]
        Right p -> return [[i|-Dwebdriver.gecko.driver=#{p}|]
                          -- , [i|-Dwebdriver.gecko.logfile=#{webdriverRoot </> "geckodriver.log"}|]
                          -- , [i|-Dwebdriver.gecko.verboseLogging=true|]
                          ]
    W.Chrome {} -> do
      obtainChromeDriver toolsRoot chromeDriverToUse >>= \case
        Left err -> error [i|Failed to obtain chromedriver: '#{err}'|]
        Right p -> return [[i|-Dwebdriver.chrome.driver=#{p}|]
                          , [i|-Dwebdriver.chrome.logfile=#{webdriverRoot </> "chromedriver.log"}|]
                          , [i|-Dwebdriver.chrome.verboseLogging=true|]]
    x -> error [i|Browser #{x} is not supported yet|]

  debug [i|driverArgs: #{driverArgs}|]

  (maybeXvfbSession, javaEnv) <- case runMode of
#ifndef mingw32_HOST_OS
    RunInXvfb (XvfbConfig {..}) -> do
      (s, e) <- makeXvfbSession xvfbResolution xvfbStartFluxbox webdriverRoot
      return (Just s, Just e)
#endif
    _ -> return (Nothing, Nothing)

  -- Retry up to 10 times
  -- This is necessary because sometimes we get a race for the port we get from findFreePortOrException.
  -- There doesn't seem to be any way to make Selenium choose its own port.
  let policy = constantDelay 0 <> limitRetries 10
  recoverAll policy $ \retryStatus -> do
    when (rsIterNumber retryStatus > 0) $
      warn [i|Trying again to start selenium server|]

    -- Create a distinct process name
    webdriverProcessName <- ("webdriver_process_" <>) <$> (liftIO makeUUID)
    let webdriverProcessRoot = webdriverRoot </> T.unpack webdriverProcessName
    liftIO $ createDirectoryIfMissing True webdriverProcessRoot
    startWebDriver' wdOptions webdriverName webdriverProcessRoot downloadDir seleniumPath driverArgs maybeXvfbSession javaEnv

startWebDriver' wdOptions@(WdOptions {capabilities=capabilities', ..}) webdriverName webdriverRoot downloadDir seleniumPath driverArgs maybeXvfbSession javaEnv = do
  port <- liftIO findFreePortOrException
  let wdCreateProcess = (proc "java" (driverArgs <> ["-jar", seleniumPath
                                                    , "-port", show port])) { env = javaEnv }

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
    liftIO (T.readFile seleniumErrPath) >>= \case
      t | readyMessage `T.isInfixOf` t -> return True
      _ -> liftIO (T.readFile seleniumOutPath) >>= \case
        t | readyMessage `T.isInfixOf` t -> return True
        _ -> return False
  unless success $ liftIO $ do
    interruptProcessGroupOf p >> waitForProcess p
    error [i|Selenium server failed to start after 60 seconds|]

  capabilities <- configureHeadlessCapabilities wdOptions runMode capabilities'
                  >>= configureDownloadCapabilities downloadDir

  -- Make the WebDriver
  WebDriver <$> pure (T.unpack webdriverName)
            <*> pure (hout, herr, p, seleniumOutPath, seleniumErrPath, maybeXvfbSession)
            <*> pure wdOptions
            <*> liftIO (newMVar mempty)
            <*> pure (def { W.wdPort = fromIntegral port
                          , W.wdCapabilities = capabilities
                          , W.wdHTTPManager = httpManager
                          , W.wdHTTPRetryCount = httpRetryCount
                          })
            <*> pure downloadDir

-- | TODO: expose this as an option
gracePeriod :: Int
gracePeriod = 30000000

stopWebDriver :: Constraints m => WebDriver -> m ()
stopWebDriver (WebDriver {wdWebDriver=(hout, herr, h, _, _, maybeXvfbSession)}) = do
  gracefullyStopProcess h gracePeriod
  liftIO $ hClose hout
  liftIO $ hClose herr

  whenJust maybeXvfbSession $ \(XvfbSession {..}) -> do
    whenJust xvfbFluxboxProcess $ \p -> do
      gracefullyStopProcess p gracePeriod

    gracefullyStopProcess xvfbProcess gracePeriod

-- * Util

seleniumOutFileName, seleniumErrFileName :: FilePath
seleniumOutFileName = "stdout.txt"
seleniumErrFileName = "stderr.txt"

-- | Add headless configuration to the Chrome browser
configureHeadlessCapabilities :: Constraints m => WdOptions -> RunMode -> W.Capabilities -> m W.Capabilities
configureHeadlessCapabilities wdOptions (RunHeadless (HeadlessConfig {..})) caps@(W.Capabilities {W.browser=browser@(W.Chrome {..})}) = do
  headlessArg <- liftIO (detectChromeVersion (chromeBinaryPath wdOptions)) >>= \case
    Left err -> do
      warn [i|Couldn't determine chrome version when configuring headless capabilities (err: #{err}); passing --headless|]
      return "--headless"
    Right (ChromeVersion (major, _, _, _))
      -- See https://www.selenium.dev/blog/2023/headless-is-going-away/
      | major >= 110 -> return "--headless=new"
      | otherwise -> return "--headless"

  let browser' = browser { W.chromeOptions = headlessArg:resolution:chromeOptions }

  return (caps { W.browser = browser' })

  where
    resolution = [i|--window-size=#{w},#{h}|]
    (w, h) = fromMaybe (1920, 1080) headlessResolution

-- | Add headless configuration to the Firefox capabilities
configureHeadlessCapabilities _ (RunHeadless (HeadlessConfig {..})) caps@(W.Capabilities {W.browser=(W.Firefox {..}), W.additionalCaps=ac}) = return (caps { W.additionalCaps = additionalCaps })
  where
    additionalCaps = case L.findIndex (\x -> fst x == "moz:firefoxOptions") ac of
      Nothing -> ("moz:firefoxOptions", A.object [("args", A.Array ["-headless"])]) : ac
      Just i -> let ffOptions' = snd (ac !! i)
                               & ensureKeyExists "args" (A.Array [])
                               & ((key "args" . _Array) %~ addHeadlessArg) in
        L.nubBy (\x y -> fst x == fst y) (("moz:firefoxOptions", ffOptions') : ac)

    ensureKeyExists :: T.Text -> A.Value -> A.Value -> A.Value
    ensureKeyExists key _ val@(A.Object (HM.lookup (fromText key) -> Just _)) = val
    ensureKeyExists key defaultVal (A.Object m@(HM.lookup (fromText key) -> Nothing)) = A.Object (HM.insert (fromText key) defaultVal m)
    ensureKeyExists _ _ _ = error "Expected Object in ensureKeyExists"

    addHeadlessArg :: V.Vector A.Value -> V.Vector A.Value
    addHeadlessArg xs | (A.String "-headless") `V.elem` xs = xs
    addHeadlessArg xs = (A.String "-headless") `V.cons` xs

configureHeadlessCapabilities _ (RunHeadless {}) browser = error [i|Headless mode not yet supported for browser '#{browser}'|]
configureHeadlessCapabilities _ _ browser = return browser


configureDownloadCapabilities downloadDir caps@(W.Capabilities {W.browser=browser@(W.Firefox {..})}) = do
  case ffProfile of
    Nothing -> return ()
    Just _ -> liftIO $ throwIO $ userError [i|Can't support Firefox profile yet.|]

  profile <- FF.defaultProfile
    & FF.addPref "browser.download.folderList" (2 :: Int)
    & FF.addPref "browser.download.manager.showWhenStarting" False
    & FF.addPref "browser.download.dir" downloadDir
    & FF.addPref "browser.helperApps.neverAsk.saveToDisk" ("*" :: String)
    & FF.prepareProfile

  return (caps { W.browser = browser { W.ffProfile = Just profile } })
configureDownloadCapabilities downloadDir caps@(W.Capabilities {W.browser=browser@(W.Chrome {..})}) = return $ caps { W.browser=browser' }
  where
    browser' = browser { W.chromeExperimentalOptions = options }

    basePrefs :: A.Object
    basePrefs = case HM.lookup "prefs" chromeExperimentalOptions of
      Just (A.Object hm) -> hm
      Just x -> error [i|Expected chrome prefs to be object, got '#{x}'.|]
      Nothing -> mempty

    prefs :: A.Object
    prefs = basePrefs
          & foldl (.) id [HM.insert k v | (k, v) <- downloadPrefs]

    options = HM.insert "prefs" (A.Object prefs) chromeExperimentalOptions

    downloadPrefs = [("profile.default_content_setting_values.automatic_downloads", A.Number 1)
                    , ("profile.content_settings.exceptions.automatic_downloads.*.setting", A.Number 1)
                    , ("download.prompt_for_download", A.Bool False)
                    , ("download.directory_upgrade", A.Bool True)
                    , ("download.default_directory", A.String (T.pack downloadDir))]
configureDownloadCapabilities _ browser = return browser
