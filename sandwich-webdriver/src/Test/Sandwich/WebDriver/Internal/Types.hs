{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Sandwich.WebDriver.Internal.Types where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text (Text)
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb
import qualified Test.WebDriver as W
import UnliftIO.Async


-- | 'Session' is just a 'String' name.
type Session = String

-- * Labels
webdriver :: Label "webdriver" TestWebDriverContext
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
  -- The @Xvfb@ binary must be installed and on the PATH.

data WdOptions = WdOptions {
  capabilities :: W.Capabilities
  -- ^ The WebDriver capabilities to use.

  , runMode :: RunMode
  -- ^ How to handle opening the browser (in a popup window, headless, etc.).

  , httpRetryCount :: Int
  -- ^ Number of times to retry an HTTP request if it times out.

  , chromeNoSandbox :: Bool
  -- ^ Pass the --no-sandbox flag to Chrome (useful in GitHub Actions when installing Chrome via Nix).
  }

-- | How to obtain certain binaries "on demand". These may or not be needed based on 'WdOptions', so
-- they will be obtained as needed.
data OnDemandOptions = OnDemandOptions {
  -- | How to obtain ffmpeg binary.
  ffmpegToUse :: FfmpegToUse

  -- | How to obtain Xvfb binary.
  , xvfbToUse :: XvfbToUse
  }
defaultOnDemandOptions :: OnDemandOptions
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
  capabilities = W.defaultCaps
  , runMode = Normal
  , httpRetryCount = 0
  , chromeNoSandbox = False
  }

data OnDemand a =
  OnDemandNotStarted
  | OnDemandInProgress (Async a)
  | OnDemandReady a
  | OnDemandErrored Text

data TestWebDriverContext = TestWebDriverContext {
  wdName :: String
  , wdContext :: W.WebDriverContext
  , wdOptions :: WdOptions
  , wdSessionMap :: MVar (M.Map String W.Session)
  , wdDriverConfig :: W.DriverConfig
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

type WebDriverSession = (Session, W.Session)

-- | Get the 'WdOptions' associated with the 'WebDriver'.
getWdOptions :: TestWebDriverContext -> WdOptions
getWdOptions = wdOptions

-- | Get the X11 display number associated with the 'WebDriver'.
-- Only present if running in 'RunInXvfb' mode.
-- getDisplayNumber :: TestWebDriverContext -> Maybe Int
-- getDisplayNumber (TestWebDriverContext {wdWebDriver=(_, Just (XvfbSession {xvfbDisplayNum}))}) = Just xvfbDisplayNum
-- getDisplayNumber _ = Nothing

-- -- | Get the Xvfb session associated with the 'WebDriver', if present.
-- getXvfbSession :: TestWebDriverContext -> Maybe XvfbSession
-- getXvfbSession (TestWebDriverContext {wdWebDriver=(_, Just sess)}) = Just sess
-- getXvfbSession _ = Nothing

-- | Get the configured download directory for the 'WebDriver'.
getDownloadDirectory :: TestWebDriverContext -> FilePath
getDownloadDirectory = wdDownloadDir

-- | Get the name of the 'WebDriver'.
-- This corresponds to the folder that will be created to hold the log files for the 'WebDriver'.
getWebDriverName :: TestWebDriverContext -> String
getWebDriverName (TestWebDriverContext {wdName}) = wdName

instance Show XvfbSession where
  show (XvfbSession {xvfbDisplayNum}) = [i|<XVFB session with server num #{xvfbDisplayNum}>|]
