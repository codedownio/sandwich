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

import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.Default
import Data.Function ((&))
import Test.WebDriver
import qualified Test.WebDriver.Firefox.Profile as FF

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
chromeCapabilities maybeChromePath = def {
  browser = Chrome Nothing maybeChromePath ["--verbose"] [] mempty
  , additionalCaps=[("loggingPrefs", loggingPrefs)
                   , ("goog:loggingPrefs", loggingPrefs)]
  }

-- | Default capabilities for headless Chrome.
headlessChromeCapabilities :: Maybe FilePath -> Capabilities
headlessChromeCapabilities maybeChromePath = def {
  browser = Chrome Nothing maybeChromePath ["--verbose", "--headless"] [] mempty
  , additionalCaps=[("loggingPrefs", loggingPrefs)
                   , ("goog:loggingPrefs", loggingPrefs)]
  }

-- * Firefox

getDefaultFirefoxProfile :: MonadBaseControl IO m => FilePath -> m (FF.PreparedProfile FF.Firefox)
getDefaultFirefoxProfile downloadDir = do
  FF.defaultProfile
    & FF.addPref "browser.download.folderList" (2 :: Int)
    & FF.addPref "browser.download.manager.showWhenStarting" False
    & FF.addPref "browser.download.dir" downloadDir
    & FF.addPref "browser.helperApps.neverAsk.saveToDisk" ("*" :: String)
    & FF.prepareProfile

-- | Default capabilities for regular Firefox.
firefoxCapabilities :: Maybe FilePath -> Capabilities
firefoxCapabilities maybeFirefoxPath = def { browser = ff }
  where
    ff = Firefox { ffProfile = Nothing
                 , ffLogPref = LogAll
                 , ffBinary = maybeFirefoxPath
                 , ffAcceptInsecureCerts = Nothing
                 }

-- | Default capabilities for headless Firefox.
headlessFirefoxCapabilities :: Maybe FilePath -> Capabilities
headlessFirefoxCapabilities maybeFirefoxPath = def { browser=ff, additionalCaps=additionalCaps }
  where
    ff = Firefox { ffProfile = Nothing
                 , ffLogPref = LogAll
                 , ffBinary = maybeFirefoxPath
                 , ffAcceptInsecureCerts = Nothing
                 }

    additionalCaps = [("moz:firefoxOptions", A.object [("args", A.Array ["-headless"])])]
