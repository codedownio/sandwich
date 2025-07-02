{-# LANGUAGE OverloadedLists #-}

module Test.Sandwich.WebDriver.Internal.Capabilities (
  -- * Chrome
  chromeCapabilities
  , headlessChromeCapabilities

  -- * Firefox
  , firefoxCapabilities
  , headlessFirefoxCapabilities
  , getDefaultFirefoxProfile
  ) where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Maybe
import Lens.Micro
import Test.WebDriver
import Test.WebDriver.Capabilities
import Test.WebDriver.Profile

loggingPrefs :: A.Value
loggingPrefs = A.object [
  ("browser", "ALL")
  , ("client", "WARNING")
  , ("driver", "WARNING")
  , ("performance", "ALL")
  , ("server", "WARNING")
  ]

-- * Chrome

-- | Default capabilities for regular Chrome.
-- Has the "browser" log level to "ALL" so that tests can collect browser logs.
chromeCapabilities :: Maybe FilePath -> Capabilities
chromeCapabilities maybeChromePath = defaultCaps {
  _capabilitiesGoogChromeOptions = Just $ defaultChromeOptions {
    _chromeOptionsArgs = Just ["--verbose"]
    , _chromeOptionsBinary = maybeChromePath
    , _chromeOptionsPerfLoggingPrefs = Just prefs
    }
  }
  where
    prefs = case loggingPrefs of
      A.Object x -> x
      _ -> error "Impossible"

-- | Default capabilities for headless Chrome.
headlessChromeCapabilities :: Maybe FilePath -> Capabilities
headlessChromeCapabilities maybeChromePath = chromeCapabilities maybeChromePath
  & over (capabilitiesGoogChromeOptions . _Just . chromeOptionsArgs) (Just . ("--headless" :) . fromMaybe [])

-- * Firefox

getDefaultFirefoxProfile :: MonadIO m => FilePath -> m (PreparedProfile Firefox)
getDefaultFirefoxProfile downloadDir = do
  defaultFirefoxProfile
    & addPref "browser.download.folderList" (2 :: Int)
    & addPref "browser.download.manager.showWhenStarting" False
    & addPref "browser.download.dir" downloadDir
    & addPref "browser.helperApps.neverAsk.saveToDisk" ("*" :: String)
    & prepareFirefoxProfile

-- | Default capabilities for regular Firefox.
firefoxCapabilities :: Maybe FilePath -> Capabilities
firefoxCapabilities maybeFirefoxPath = defaultCaps {
  _capabilitiesMozFirefoxOptions = Just $ defaultFirefoxOptions {
    _firefoxOptionsBinary = maybeFirefoxPath
    , _firefoxOptionsLog = Just (FirefoxLogLevel FirefoxLogLevelTypeInfo)
    }
  }

-- | Default capabilities for headless Firefox.
headlessFirefoxCapabilities :: Maybe FilePath -> Capabilities
headlessFirefoxCapabilities maybeFirefoxPath = defaultCaps {
  _capabilitiesMozFirefoxOptions = Just $ defaultFirefoxOptions {
    _firefoxOptionsBinary = maybeFirefoxPath
    , _firefoxOptionsArgs = Just ["-headless"]
    , _firefoxOptionsLog = Just (FirefoxLogLevel FirefoxLogLevelTypeInfo)
    }
  }
