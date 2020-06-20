{-# LANGUAGE TypeFamilies, InstanceSigs, RecordWildCards, ScopedTypeVariables, QuasiQuotes, Rank2Types, NamedFieldPuns #-}

module Test.Sandwich.WebDriver.Internal.Types where

import Control.Concurrent.MVar
import Control.Exception
import qualified Data.Aeson as A
import Data.Default
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import System.IO
import System.Process
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Session as W

type Browser = String

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
             | RunHeadless
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

  , runMode :: RunMode
  -- ^ How to handle opening the browser (in a popup window, headless, etc.)
  }

data XvfbConfig = XvfbConfig {
  xvfbResolution :: Maybe (Int, Int)
  -- ^ Resolution for the virtual screen. Defaults to (1920, 1080)
  }

instance Default XvfbConfig where
  def = XvfbConfig Nothing

defaultWdOptions :: FilePath -> WdOptions
defaultWdOptions toolsRoot = WdOptions toolsRoot def OnException mempty Normal

type SaveLogSettings = M.Map W.LogType (W.LogEntry -> Bool, W.LogEntry -> T.Text, W.LogEntry -> Bool)

data WdSession = WdSession { wdLabels :: [String]
                           , wdWebDriver :: (Handle, Handle, ProcessHandle, FilePath, FilePath, Maybe XvfbSession)
                           , wdOptions :: WdOptions
                           , wdSessionMap :: MVar (M.Map Browser W.WDSession)
                           , wdFailureCounter :: MVar Int
                           , wdTimingInfo :: MVar A.Value
                           , wdSaveBrowserLogs :: MVar SaveLogSettings
                           , wdConfig :: W.WDConfig }

data InvalidLogsException = InvalidLogsException [W.LogEntry]
  deriving (Show)

instance Exception InvalidLogsException

data XvfbSession = XvfbSession { xvfbDisplayNum :: Int
                               , xvfbXauthority :: FilePath
                               , xvfbDimensions :: (Int, Int)
                               , xvfbProcess :: ProcessHandle }

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
