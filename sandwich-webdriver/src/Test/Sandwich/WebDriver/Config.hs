
-- | Configuration types for WebDriver servers, Xvfb mode, browser capabilities, etc.

module Test.Sandwich.WebDriver.Config (
  -- * Main options
  WdOptions
  , defaultWdOptions
  , runMode
  , capabilities
  , httpManager
  , httpRetryCount
  , saveSeleniumMessageHistory

  -- * Accessors for the 'WebDriver' context
  , getWdOptions
  , getDisplayNumber
  , getDownloadDirectory
  , getWebDriverName

  -- * Xvfb mode
  , XvfbConfig
  , defaultXvfbConfig
  , xvfbResolution
  , xvfbStartFluxbox

  -- * Headless mode
  , HeadlessConfig
  , defaultHeadlessConfig
  , headlessResolution

  -- * Browser capabilities
  , chromeCapabilities
  , headlessChromeCapabilities
  , firefoxCapabilities
  , headlessFirefoxCapabilities

  -- * Types
  , WhenToSave(..)
  , RunMode(..)
  , browserDependencies
  , BrowserDependenciesSpec(..)
  , BrowserDependencies(..)
  , HasBrowserDependencies
  ) where

import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
