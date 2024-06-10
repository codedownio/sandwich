{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.WebDriver.Internal.BrowserDependencies where

import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver.Internal.Types


-- * All dependencies

data WebDriverDependencies = WebDriverDependencies {
  -- | Path to "java" binary to use to start Selenium. If not provided, we'll search the PATH.
  webDriverDependencyJava :: Maybe FilePath
  -- | How to obtain a selenium.jar file.
  , webDriverDependencySelenium :: SeleniumToUse
  -- | Browser/driver dependencies.
  , webDriverDependencyBrowser :: BrowserDependenciesSpec
  }

data BrowserDependenciesSpec = BrowserDependenciesSpecChrome {
  browserDependenciesSpecChromeChrome :: ChromeToUse
  , browserDependenciesSpecChromeChromeDriver :: ChromeDriverToUse
  }
  | BrowserDependenciesSpecFirefox {
      browserDependenciesSpecFirefoxFirefox :: FirefoxToUse
      , browserDependenciesSpecFirefoxGeckodriver :: GeckoDriverToUse
      }

defaultWebDriverDependencies = WebDriverDependencies {
  webDriverDependencyJava = Nothing
  , webDriverDependencySelenium = DownloadSeleniumDefault
  , webDriverDependencyBrowser = BrowserDependenciesSpecFirefox UseFirefoxFromPath DownloadGeckoDriverAutodetect
  }

-- * Browser dependencies

data BrowserDependencies = BrowserDependenciesChrome {
  browserDependenciesChromeChrome :: FilePath
  , browserDependenciesChromeChromedriver :: FilePath
  }
  | BrowserDependenciesFirefox {
      browserDependenciesFirefoxFirefox :: FilePath
      , browserDependenciesFirefoxGeckodriver :: FilePath
      }
  deriving (Show)

browserDependencies :: Label "browserDependencies" BrowserDependencies
browserDependencies = Label

type HasBrowserDependencies context = HasLabel context "browserDependencies" BrowserDependencies

introduceBrowserDependencies :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context, HasSomeCommandLineOptions context
  ) => SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m () -> SpecFree context m ()
introduceBrowserDependencies = introduce "Introduce browser dependencies" browserDependencies alloc (const $ return ())
  where
    alloc = do
      SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})}) <- getSomeCommandLineOptions

      deps <- case optFirefox of
        Just UseChrome ->
          BrowserDependenciesChrome <$> getBinaryViaNixPackage @"google-chrome-stable" "google-chrome"
                                    <*> getBinaryViaNixPackage @"chromedriver" "chromedriver"
        Just UseFirefox ->
          BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
                                     <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"
        _ ->
          BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
                                     <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"
      debug [i|Got browser dependencies: #{deps}|]
      return deps
