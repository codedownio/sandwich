-- |

module Test.Sandwich.WebDriver.Internal.Capabilities (
  chromeCapabilities
  , headlessChromeCapabilities
  ) where

import qualified Data.Aeson as A
import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Test.WebDriver

loggingPrefs :: A.Value
loggingPrefs = A.object [("browser", "ALL")
                        , ("client", "WARNING")
                        , ("driver", "WARNING")
                        , ("performance", "ALL")
                        , ("server", "WARNING")
                        ]

-- | Default capabilities for regular Chrome.
-- It's important to set the "browser" log level to "ALL" so that tests can collect browser logs.
chromeCapabilities :: Capabilities
chromeCapabilities =
  def {browser=Chrome Nothing Nothing args [] chromePrefs
      , additionalCaps=[("loggingPrefs", loggingPrefs)
                       , ("goog:loggingPrefs", loggingPrefs)]
      }
  where args = ["--verbose"]

-- | Default capabilities for headless Chrome.
headlessChromeCapabilities :: Capabilities
headlessChromeCapabilities =
  def {browser=Chrome Nothing Nothing args [] chromePrefs
      , additionalCaps=[("loggingPrefs", loggingPrefs)
                       , ("goog:loggingPrefs", loggingPrefs)]
      }
  where args = ["--verbose", "--headless"]

chromePrefs :: HM.HashMap T.Text A.Value
chromePrefs = HM.fromList [
  ("prefs", A.object [("profile.default_content_setting_values.automatic_downloads", A.Number 1)
                     , ("profile.content_settings.exceptions.automatic_downloads.*.setting", A.Number 1)
                     , ("download.prompt_for_download", A.Bool False)
                     , ("download.directory_upgrade", A.Bool True)
                     , ("download.default_directory", "/tmp")])
  ]

-- getFirefoxCapabilities :: IO Capabilities
-- getFirefoxCapabilities = do
--   profile <- prepareProfile (addPref "webdriver.log.file" ("/tmp/firefox_console" :: String) defaultProfile)
--   let ffAcceptInsecureCerts = Nothing
--   let ff = Firefox (Just profile) LogAll Nothing ffAcceptInsecureCerts
--   return $ def {browser=ff}
