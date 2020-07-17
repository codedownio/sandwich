{-# LANGUAGE TypeFamilies, InstanceSigs, RecordWildCards, ScopedTypeVariables, QuasiQuotes, Rank2Types, NamedFieldPuns, DataKinds, ConstraintKinds #-}

module Test.Sandwich.WebDriver.Internal.Types where

import Control.Concurrent.MVar
import Control.Exception
import Data.Default
import Data.IORef
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import System.IO
import System.Process
import Test.Sandwich
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Session as W

type Browser = String

-- * Labels
webdriver = Label :: Label "webdriver" WdSession
webdriverSession = Label :: Label "webdriverSession" (IORef W.WDSession)

type HasWebDriver context wd = (HasLabel context "webdriver" WdSession, W.WebDriver (ExampleT context wd))

-- TODO: remove
class HasWdSession a where
  getWdSession :: a -> WdSession

instance HasWdSession WdSession where
  getWdSession = id

type ToolsRoot = FilePath

data WhenToSave = Always | OnException | Never deriving (Show, Eq)

-- | Headless and Xvfb modes are useful because they allow you to run tests in the background, without popping up browser windows.
-- This is useful for development or for running on a CI server, and is also more reproducible since the screen resolution can be fixed.
-- In addition, Xvfb mode allows videos to be recorded of tests.
data RunMode = Normal
             -- ^ Normal Selenium behavior; will pop up a web browser.
             | RunHeadless HeadlessConfig
             -- ^ Run with a headless browser. Supports screenshots but videos will be black.
             | RunInXvfb XvfbConfig
             -- ^ Run inside <https://en.wikipedia.org/wiki/Xvfb Xvfb> so that tests run in their own X11 display.
             -- xvfb-run script must be installed and on the PATH.

data WdOptions = WdOptions {
  toolsRoot :: ToolsRoot
  -- ^ Folder where any necessary binaries (chromedriver, Selenium, etc.) will be downloaded if needed. Required.

  , capabilities :: W.Capabilities
  -- ^ The WebDriver capabilities to use

  , saveSeleniumMessageHistory :: WhenToSave
  -- ^ When to save a record of Selenium requests and responses

  , saveLogSettings :: SaveLogSettings
  -- ^ When to save a record of Selenium requests and responses

  , seleniumToUse :: SeleniumToUse
  -- ^ Which Selenium server JAR file to use

  , chromeDriverToUse :: ChromeDriverToUse
  -- ^ Which chromedriver executable to use

  , runMode :: RunMode
  -- ^ How to handle opening the browser (in a popup window, headless, etc.)
  }

-- | How to obtain the Selenium server JAR file
data SeleniumToUse =
  DownloadSeleniumFrom String
  -- ^ Download selenium from the given URL to the 'toolsRoot'
  | DownloadSeleniumDefault
  -- ^ Download selenium from a default location to the 'toolsRoot'
  | UseSeleniumAt FilePath
  -- ^ Use the JAR file at the given path

data ChromeDriverToUse =
  DownloadChromeDriverFrom String
  -- ^ Download chromedriver from the given URL to the 'toolsRoot'
  | DownloadChromeDriverVersion ChromeDriverVersion
  -- ^ Download the given chromedriver version to the 'toolsRoot'
  | DownloadChromeDriverAutodetect
  -- ^ Autodetect chromedriver to use based on the Chrome version and download it to the 'toolsRoot'
  | UseChromeDriverAt FilePath
  -- ^ Use the chromedriver at the given path

newtype ChromeVersion = ChromeVersion (Int, Int, Int, Int)
newtype ChromeDriverVersion = ChromeDriverVersion (Int, Int, Int, Int)

data HeadlessConfig = HeadlessConfig {
  headlessResolution :: Maybe (Int, Int)
  -- ^ Resolution for the headless browser. Defaults to (1920, 1080)
  }

defaultHeadlessConfig = HeadlessConfig Nothing

data XvfbConfig = XvfbConfig {
  xvfbResolution :: Maybe (Int, Int)
  -- ^ Resolution for the virtual screen. Defaults to (1920, 1080)

  , xvfbStartFluxbox :: Bool
  -- ^ Whether to start fluxbox window manager to go with the Xvfb session. fluxbox must be on the path
  }

defaultXvfbConfig = XvfbConfig Nothing False


defaultWdOptions :: FilePath -> WdOptions
defaultWdOptions toolsRoot = WdOptions toolsRoot def OnException mempty DownloadSeleniumDefault DownloadChromeDriverAutodetect Normal

type SaveLogSettings = M.Map W.LogType (W.LogEntry -> Bool, W.LogEntry -> T.Text, W.LogEntry -> Bool)

data WdSession = WdSession { wdName :: String
                           , wdWebDriver :: (Handle, Handle, ProcessHandle, FilePath, FilePath, Maybe XvfbSession)
                           , wdOptions :: WdOptions
                           , wdSessionMap :: MVar (M.Map Browser W.WDSession)
                           , wdFailureCounter :: MVar Int
                           , wdSaveBrowserLogs :: MVar SaveLogSettings
                           , wdConfig :: W.WDConfig }

data InvalidLogsException = InvalidLogsException [W.LogEntry]
  deriving (Show)

instance Exception InvalidLogsException

data XvfbSession = XvfbSession { xvfbDisplayNum :: Int
                               , xvfbXauthority :: FilePath
                               , xvfbDimensions :: (Int, Int)
                               , xvfbProcess :: ProcessHandle
                               , xvfbOut :: Handle
                               , xvfbErr :: Handle
                               , xvfbFluxboxProcess :: Maybe ProcessHandle }

getDisplayNumber :: WdSession -> Maybe Int
getDisplayNumber (WdSession {wdWebDriver=(_, _, _, _, _, Just (XvfbSession {xvfbDisplayNum}))}) = Just xvfbDisplayNum
getDisplayNumber _ = Nothing

instance Show XvfbSession where
  show (XvfbSession {xvfbDisplayNum}) = [i|<XVFB session with server num #{xvfbDisplayNum}>|]

-- * Video stuff

fastX11VideoOptions = ["-an"
                      , "-r", "30"
                      , "-vcodec"
                      , "libxvid"
                      , "-qscale:v", "1"
                      , "-threads", "0"]

qualityX11VideoOptions = ["-an"
                         , "-r", "30"
                         , "-vcodec", "libx264"
                         , "-preset", "veryslow"
                         , "-crf", "0"
                         , "-threads", "0"]

defaultAvfoundationOptions = ["-r", "30"
                             , "-an"
                             , "-vcodec", "libxvid"
                             , "-qscale:v", "1"
                             , "-threads", "0"]

defaultGdigrabOptions = ["-framerate", "30"]

data VideoSettings = VideoSettings { x11grabOptions :: [String]
                                   -- ^ Arguments to x11grab, used with Linux.
                                   , avfoundationOptions :: [String]
                                   -- ^ Arguments to avfoundation, used with OS X.
                                   , gdigrabOptions :: [String]
                                   -- ^ Arguments to gdigrab, used with Windows.
                                   , hideMouseWhenRecording :: Bool
                                   -- ^ Hide the mouse while recording video. Linux and Windows only.
                                   }

instance Default VideoSettings where
  def = VideoSettings { x11grabOptions = fastX11VideoOptions
                      , avfoundationOptions = defaultAvfoundationOptions
                      , gdigrabOptions = defaultGdigrabOptions
                      , hideMouseWhenRecording = False }
