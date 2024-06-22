
module Test.Sandwich.WebDriver.Config (
  -- * Main options
  WdOptions
  , defaultWdOptions
  , runMode
  , capabilities
  , httpManager
  , httpRetryCount
  , saveSeleniumMessageHistory
  , WhenToSave(..)
  , RunMode(..)

  -- * The WebDriver context
  , WebDriver
  , getWdOptions
  , getDisplayNumber
  , getWebDriverName

  -- ** Xvfb mode
  , XvfbConfig
  , defaultXvfbConfig
  , xvfbResolution
  , xvfbStartFluxbox

  -- ** Headless mode
  , HeadlessConfig
  , defaultHeadlessConfig
  , headlessResolution

  -- * Dependency obtaining options
  , SeleniumToUse(..)
  , BrowserDependenciesSpec(..)
  , ChromeToUse(..)
  , ChromeDriverToUse(..)
  , FirefoxToUse(..)
  , GeckoDriverToUse(..)
  , GeckoDriverVersion(..)
  , XvfbDependenciesSpec(..)
  , XvfbToUse(..)
  , FfmpegToUse(..)
  , BrowserDependencies(..)

  -- * Browser capabilities
  , chromeCapabilities
  , headlessChromeCapabilities
  , firefoxCapabilities
  , headlessFirefoxCapabilities
  ) where

import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb
import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
