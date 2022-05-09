
module Test.Sandwich.WebDriver.Config (
  -- * Main options
  WdOptions
  , defaultWdOptions
  , runMode
  , downloadDir
  , seleniumToUse
  , chromeBinaryPath
  , chromeDriverToUse
  , firefoxBinaryPath
  , geckoDriverToUse
  , capabilities
  , httpManager
  , httpRetryCount
  , saveSeleniumMessageHistory

  -- * Run mode constructors
  , RunMode(..)

  -- ** Xvfb mode
  , XvfbConfig
  , defaultXvfbConfig
  , xvfbResolution
  , xvfbStartFluxbox

  -- ** Headless mode
  , HeadlessConfig
  , defaultHeadlessConfig
  , headlessResolution

  -- * Binary fetching options
  , SeleniumToUse(..)
  , ChromeDriverToUse(..)
  , GeckoDriverToUse(..)

  -- * Miscellaneous constructors
  , WhenToSave(..)

  -- * Manually obtaining binaries
  , obtainSelenium
  , obtainChromeDriver
  , obtainGeckoDriver

  -- * Browser capabilities
  , chromeCapabilities
  , headlessChromeCapabilities
  , firefoxCapabilities
  , headlessFirefoxCapabilities

  ) where

import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.Types
