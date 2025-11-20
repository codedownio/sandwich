
-- | Configuration types for WebDriver servers, Xvfb mode, browser capabilities, etc.

module Test.Sandwich.WebDriver.Config (
  -- * Main options
  WdOptions
  , defaultWdOptions
  , runMode
  , httpRetryCount
  , geckodriverExtraFlags
  , chromedriverExtraFlags
  , modifyCapabilities

  -- * Accessors for the 'WebDriver' context
  , getWdOptions
  -- , getDisplayNumber
  , getDownloadDirectory
  , getWebDriverName
  -- , getXvfbSession

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
  -- ** BrowserDependencies
  , browserDependencies
  , BrowserDependenciesSpec(..)
  , BrowserDependencies(..)
  , HasBrowserDependencies
  -- ** Xvfb
  , XvfbSession(..)
  -- ** Miscellaneous
  , WhenToSave(..)
  , RunMode(..)
  ) where

import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
