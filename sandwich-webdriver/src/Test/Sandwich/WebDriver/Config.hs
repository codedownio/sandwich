
module Test.Sandwich.WebDriver.Config (
  -- * Main options
  WdOptions
  , defaultWdOptions
  , runMode
  , capabilities
  , httpManager
  , httpRetryCount
  , saveSeleniumMessageHistory

  -- * Browser options
  , BrowserDependencies(..)

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

  -- * Browser capabilities
  , chromeCapabilities
  , headlessChromeCapabilities
  , firefoxCapabilities
  , headlessFirefoxCapabilities
  ) where

import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.BrowserDependencies
import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.Types
