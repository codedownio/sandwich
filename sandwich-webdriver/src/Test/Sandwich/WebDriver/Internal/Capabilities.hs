{-# LANGUAGE OverloadedLists #-}
-- |

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
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Test.WebDriver
import qualified Test.WebDriver.Firefox.Profile as FF

loggingPrefs :: A.Value
loggingPrefs = A.object [("browser", "ALL")
                        , ("client", "WARNING")
                        , ("driver", "WARNING")
                        , ("performance", "ALL")
                        , ("server", "WARNING")
                        ]

-- * Chrome

-- | Default capabilities for regular Chrome.
-- Has the "browser" log level to "ALL" so that tests can collect browser logs.
chromeCapabilities :: Maybe FilePath -> Maybe FilePath -> Capabilities
chromeCapabilities maybeChromePath maybeDownloadDir =
  def {browser=Chrome Nothing maybeChromePath args [] (chromePrefs maybeDownloadDir)
      , additionalCaps=[("loggingPrefs", loggingPrefs)
                       , ("goog:loggingPrefs", loggingPrefs)]
      }
  where args = ["--verbose"]

-- | Default capabilities for headless Chrome.
headlessChromeCapabilities :: Maybe FilePath -> Maybe FilePath -> Capabilities
headlessChromeCapabilities maybeChromePath maybeDownloadDir =
  def {browser=Chrome Nothing maybeChromePath args [] (chromePrefs maybeDownloadDir)
      , additionalCaps=[("loggingPrefs", loggingPrefs)
                       , ("goog:loggingPrefs", loggingPrefs)]
      }
  where args = ["--verbose", "--headless"]

chromePrefs :: Maybe FilePath -> HM.HashMap T.Text A.Value
chromePrefs maybeDownloadDir = HM.fromList [
  ("prefs", A.object [("profile.default_content_setting_values.automatic_downloads", A.Number 1)
                     , ("profile.content_settings.exceptions.automatic_downloads.*.setting", A.Number 1)
                     , ("download.prompt_for_download", A.Bool False)
                     , ("download.directory_upgrade", A.Bool True)
                     , ("download.default_directory", A.String (T.pack (fromMaybe "/tmp" maybeDownloadDir)))])
  ]

-- * Firefox

getDefaultFirefoxProfile :: MonadBaseControl IO m => FilePath -> m (FF.PreparedProfile FF.Firefox)
getDefaultFirefoxProfile downloadDir = do
  FF.defaultProfile
    & FF.addPref "browser.download.folderList" (2 :: Int)
    & FF.addPref "browser.download.manager.showWhenStarting" False
    & FF.addPref "browser.download.dir" downloadDir
    & FF.prepareProfile

-- | Default capabilities for regular Firefox.
firefoxCapabilities :: Maybe FilePath -> Maybe (FF.PreparedProfile FF.Firefox) -> Capabilities
firefoxCapabilities maybeFirefoxPath maybeProfile = def { browser=ff }
  where
    ff = Firefox { ffProfile = maybeProfile
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
