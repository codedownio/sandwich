{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.WebDriver.Internal.BrowserDependencies (
  WebDriverDependencies(..)
  , BrowserDependenciesSpec(..)

  , defaultWebDriverDependencies
  , nixFirefoxWebDriverDependencies
  , nixChromeWebDriverDependencies

  , BrowserDependencies(..)
  , browserDependencies
  , HasBrowserDependencies

  , getBrowserDependencies
  , introduceBrowserDependenciesViaNix
  , fillInCapabilitiesAndGetDriverArgs
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Types
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W


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
  , webDriverDependencySelenium = DownloadSeleniumDefault "/tmp/tools"
  , webDriverDependencyBrowser = BrowserDependenciesSpecFirefox UseFirefoxFromPath (DownloadGeckoDriverAutodetect "/tmp/tools")
  }

nixFirefoxWebDriverDependencies nixContext = WebDriverDependencies {
  webDriverDependencyJava = Nothing
  , webDriverDependencySelenium = UseSeleniumFromNixpkgs nixContext
  , webDriverDependencyBrowser = BrowserDependenciesSpecFirefox (UseFirefoxFromNixpkgs nixContext) (UseGeckoDriverFromNixpkgs nixContext)
  }

nixChromeWebDriverDependencies nixContext = WebDriverDependencies {
  webDriverDependencyJava = Nothing
  , webDriverDependencySelenium = UseSeleniumFromNixpkgs nixContext
  , webDriverDependencyBrowser = BrowserDependenciesSpecChrome (UseChromeFromNixpkgs nixContext) (UseChromeDriverFromNixpkgs nixContext)
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

getBrowserDependencies :: (
  MonadUnliftIO m, MonadLogger m, MonadFail m
  , MonadReader context m, HasBaseContext context
  ) => BrowserDependenciesSpec -> m BrowserDependencies
getBrowserDependencies BrowserDependenciesSpecChrome {..} = do
  chrome <- exceptionOnLeft $ obtainChrome browserDependenciesSpecChromeChrome
  chromeDriver <- exceptionOnLeft $ obtainChromeDriver browserDependenciesSpecChromeChromeDriver
  return $ BrowserDependenciesChrome chrome chromeDriver
getBrowserDependencies (BrowserDependenciesSpecFirefox {..}) = do
  firefox <- exceptionOnLeft $ obtainFirefox browserDependenciesSpecFirefoxFirefox
  geckoDriver <- exceptionOnLeft $ obtainGeckoDriver browserDependenciesSpecFirefoxGeckodriver
  return $ BrowserDependenciesFirefox firefox geckoDriver

introduceBrowserDependenciesViaNix :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context, HasSomeCommandLineOptions context
  ) => SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m () -> SpecFree context m ()
introduceBrowserDependenciesViaNix = introduce "Introduce browser dependencies" browserDependencies alloc (const $ return ())
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

fillInCapabilitiesAndGetDriverArgs webdriverRoot capabilities'' = getContext browserDependencies >>= \case
  BrowserDependenciesFirefox {..} -> do
    let args = [
          [i|-Dwebdriver.gecko.driver=#{browserDependenciesFirefoxGeckodriver}|]
          -- , [i|-Dwebdriver.gecko.logfile=#{webdriverRoot </> "geckodriver.log"}|]
          -- , [i|-Dwebdriver.gecko.verboseLogging=true|]
          ]
    let capabilities' = capabilities'' {
          W.browser = W.firefox { W.ffBinary = Just browserDependenciesFirefoxFirefox }
          }
    return (args, capabilities')
  BrowserDependenciesChrome {..} -> do
    let args = [
          [i|-Dwebdriver.chrome.driver=#{browserDependenciesChromeChromedriver}|]
          , [i|-Dwebdriver.chrome.logfile=#{webdriverRoot </> "chromedriver.log"}|]
          , [i|-Dwebdriver.chrome.verboseLogging=true|]
          ]
    let capabilities' = capabilities'' {
          W.browser = W.chrome { W.chromeBinary = Just browserDependenciesChromeChrome }
          }
    return (args, capabilities')
