{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.Sandwich.WebDriver.Internal.Types where

import Control.Concurrent.MVar
import Control.Exception
import Data.Default
import Data.IORef
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Session as W
import UnliftIO.Async


-- | 'Session' is just a 'String' name.
type Session = String

-- * Labels
webdriver :: Label "webdriver" WebDriver
webdriver = Label

webdriverSession :: Label "webdriverSession" WebDriverSession
webdriverSession = Label

type ToolsRoot = FilePath

data WhenToSave = Always | OnException | Never deriving (Show, Eq)

-- | Headless and Xvfb modes are useful because they allow you to run tests in the background, without popping up browser windows.
-- This is useful for development or for running on a CI server, and is also more reproducible since the screen resolution can be fixed.
-- In addition, Xvfb mode allows videos to be recorded of tests.
data RunMode =
  Normal
  -- ^ Normal Selenium behavior; will pop up a web browser.
  | RunHeadless HeadlessConfig
  -- ^ Run with a headless browser. Supports screenshots but videos will be black.
  | RunInXvfb XvfbConfig
  -- ^ Run inside <https://en.wikipedia.org/wiki/Xvfb Xvfb> so that tests run in their own X11 display.
  -- xvfb-run script must be installed and on the PATH.

data WdOptions = WdOptions {
  capabilities :: W.Capabilities
  -- ^ The WebDriver capabilities to use.

  , saveSeleniumMessageHistory :: WhenToSave
  -- ^ When to save a record of Selenium requests and responses.

  , runMode :: RunMode
  -- ^ How to handle opening the browser (in a popup window, headless, etc.).

  , httpManager :: Maybe Manager
  -- ^ HTTP manager for making requests to Selenium. If not provided, one will be created for each session.

  , httpRetryCount :: Int
  -- ^ Number of times to retry an HTTP request if it times out.
  }

-- | How to obtain certain binaries "on demand". These may or not be needed based on 'WdOptions', so
-- they will be obtained as needed.
data OnDemandOptions = OnDemandOptions {
  -- | How to obtain ffmpeg binary.
  ffmpegToUse :: FfmpegToUse

  -- | How to obtain Xvfb binary.
  , xvfbToUse :: XvfbToUse
  }
defaultOnDemandOptions = OnDemandOptions {
  ffmpegToUse = UseFfmpegFromPath
  , xvfbToUse = UseXvfbFromPath
  }

-- | Configuration for a headless browser.
data HeadlessConfig = HeadlessConfig {
  headlessResolution :: Maybe (Int, Int)
  -- ^ Resolution for the headless browser, specified as @(width, height)@. Defaults to @(1920, 1080)@.
  }

-- | Default headless config.
defaultHeadlessConfig :: HeadlessConfig
defaultHeadlessConfig = HeadlessConfig Nothing

data XvfbConfig = XvfbConfig {
  xvfbResolution :: Maybe (Int, Int)
  -- ^ Resolution for the virtual screen. Defaults to (1920, 1080)

  , xvfbStartFluxbox :: Bool
  -- ^ Whether to start fluxbox window manager to go with the Xvfb session. @fluxbox@ must be on the path.
  }

-- | Default Xvfb settings.
defaultXvfbConfig :: XvfbConfig
defaultXvfbConfig = XvfbConfig Nothing False

-- | The default 'WdOptions' object.
-- You should start with this and modify it using the accessors.
defaultWdOptions :: WdOptions
defaultWdOptions = WdOptions {
  capabilities = def
  , saveSeleniumMessageHistory = OnException
  , runMode = Normal
  , httpManager = Nothing
  , httpRetryCount = 0
  }

data OnDemand a =
  OnDemandNotStarted
  | OnDemandInProgress (Async a)
  | OnDemandReady a
  | OnDemandErrored Text

data WebDriver = WebDriver {
  wdName :: String
  , wdWebDriver :: (ProcessHandle, Maybe XvfbSession)
  , wdOptions :: WdOptions
  , wdSessionMap :: MVar (M.Map Session W.WDSession)
  , wdConfig :: W.WDConfig
  , wdDownloadDir :: FilePath

  , wdFfmpegToUse :: FfmpegToUse
  , wdFfmpeg :: MVar (OnDemand FilePath)

  , wdXvfbToUse :: XvfbToUse
  , wdXvfb :: MVar (OnDemand FilePath)
  }

data InvalidLogsException = InvalidLogsException [W.LogEntry]
  deriving (Show)

instance Exception InvalidLogsException

data XvfbSession = XvfbSession {
  xvfbDisplayNum :: Int
  , xvfbXauthority :: FilePath
  , xvfbDimensions :: (Int, Int)
  , xvfbProcess :: ProcessHandle
  , xvfbFluxboxProcess :: Maybe ProcessHandle
  }

type WebDriverSession = (Session, IORef W.WDSession)

-- | Get the 'WdOptions' associated with the 'WebDriver'.
getWdOptions :: WebDriver -> WdOptions
getWdOptions = wdOptions

-- | Get the X11 display number associated with the 'WebDriver'.
-- Only present if running in 'RunInXvfb' mode.
getDisplayNumber :: WebDriver -> Maybe Int
getDisplayNumber (WebDriver {wdWebDriver=(_, Just (XvfbSession {xvfbDisplayNum}))}) = Just xvfbDisplayNum
getDisplayNumber _ = Nothing

-- | Get the Xvfb session associated with the 'WebDriver', if present.
getXvfbSession :: WebDriver -> Maybe XvfbSession
getXvfbSession (WebDriver {wdWebDriver=(_, Just sess)}) = Just sess
getXvfbSession _ = Nothing

-- | Get the configured download directory for the 'WebDriver'.
getDownloadDirectory :: WebDriver -> FilePath
getDownloadDirectory = wdDownloadDir

-- | Get the name of the 'WebDriver'.
-- This corresponds to the folder that will be created to hold the log files for the 'WebDriver'.
getWebDriverName :: WebDriver -> String
getWebDriverName (WebDriver {wdName}) = wdName

instance Show XvfbSession where
  show (XvfbSession {xvfbDisplayNum}) = [i|<XVFB session with server num #{xvfbDisplayNum}>|]
