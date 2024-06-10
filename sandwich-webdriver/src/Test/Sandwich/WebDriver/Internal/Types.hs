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
import Network.HTTP.Client (Manager)
import System.Process
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Session as W

-- | 'Session' is just a 'String' name.
type Session = String

-- * Labels
webdriver :: Label "webdriver" WebDriver
webdriver = Label

webdriverSession :: Label "webdriverSession" WebDriverSession
webdriverSession = Label

type WebDriverContext context wd = (HasLabel context "webdriver" WebDriver, W.WebDriver (ExampleT context wd))

-- TODO: remove
class HasWebDriver a where
  getWebDriver :: a -> WebDriver

instance HasWebDriver WebDriver where
  getWebDriver = id

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

-- | How to obtain the Selenium server JAR file.
data SeleniumToUse =
  DownloadSeleniumFrom String
  -- ^ Download selenium from the given URL to the 'toolsRoot'
  | DownloadSeleniumDefault
  -- ^ Download selenium from a default location to the 'toolsRoot'
  | UseSeleniumAt FilePath
  -- ^ Use the JAR file at the given path
  | UseSeleniumFromNixpkgs NixContext
  -- ^ Use the Selenium in the given Nixpkgs derivation
  deriving Show

-- | How to obtain the firefox binary.
data FirefoxToUse =
  -- | Search the PATH for the "firefox" binary.
  UseFirefoxFromPath
  -- | Get Firefox from Nixpkgs
  | UseFirefoxFromNixpkgs NixContext
  deriving Show

-- | How to obtain the geckodriver binary.
data GeckoDriverToUse =
  DownloadGeckoDriverFrom String
  -- ^ Download geckodriver from the given URL to the 'toolsRoot'
  | DownloadGeckoDriverVersion GeckoDriverVersion
  -- ^ Download the given geckodriver version to the 'toolsRoot'
  | DownloadGeckoDriverAutodetect
  -- ^ Autodetect geckodriver to use based on the Firefox version and download it to the 'toolsRoot'.
  | UseGeckoDriverAt FilePath
  -- ^ Use the geckodriver at the given path
  | UseGeckoDriverFromNixpkgs NixpkgsDerivation
  -- ^ Use the geckodriver in the given Nixpkgs derivation
  deriving Show

newtype FirefoxVersion = FirefoxVersion (Int, Int, Int) deriving Show
newtype GeckoDriverVersion = GeckoDriverVersion (Int, Int, Int) deriving Show

data HeadlessConfig = HeadlessConfig {
  headlessResolution :: Maybe (Int, Int)
  -- ^ Resolution for the headless browser. Defaults to (1920, 1080)
  }

-- | Default headless config.
defaultHeadlessConfig :: HeadlessConfig
defaultHeadlessConfig = HeadlessConfig Nothing

data XvfbConfig = XvfbConfig {
  xvfbResolution :: Maybe (Int, Int)
  -- ^ Resolution for the virtual screen. Defaults to (1920, 1080)

  , xvfbStartFluxbox :: Bool
  -- ^ Whether to start fluxbox window manager to go with the Xvfb session. fluxbox must be on the path
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

data WebDriver = WebDriver {
  wdName :: String
  , wdWebDriver :: (ProcessHandle, Maybe XvfbSession)
  , wdOptions :: WdOptions
  , wdSessionMap :: MVar (M.Map Session W.WDSession)
  , wdConfig :: W.WDConfig
  , wdDownloadDir :: FilePath
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

-- | Get the name of the 'WebDriver'.
getWebDriverName :: WebDriver -> String
getWebDriverName (WebDriver {wdName}) = wdName

instance Show XvfbSession where
  show (XvfbSession {xvfbDisplayNum}) = [i|<XVFB session with server num #{xvfbDisplayNum}>|]
