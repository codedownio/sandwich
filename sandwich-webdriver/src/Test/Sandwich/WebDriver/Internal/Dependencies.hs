{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.WebDriver.Internal.Dependencies (
  WebDriverDependencies(..)
  , BrowserDependenciesSpec(..)

  , defaultWebDriverDependencies

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
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium.Types
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W


-- * All dependencies

-- | This type describes how we should obtain all the dependencies needed to launch a WebDriver session.
-- You can configure them individually.
data WebDriverDependencies = WebDriverDependencies {
  -- | Path to @java@ binary to use to start Selenium. If not provided, we'll search the PATH.
  webDriverDependencyJava :: Maybe FilePath
  -- | How to obtain a @selenium.jar@ file.
  , webDriverDependencySelenium :: SeleniumToUse
  -- | Browser/driver dependencies.
  , webDriverDependencyBrowser :: BrowserDependenciesSpec
  -- | How to obtain @Xvfb@ (used for the 'RunInXvfb' 'RunMode')
  , webDriverXvfb :: XvfbDependenciesSpec
  -- | How to obtain @ffmpeg@ (used for video recording).
  , webDriverFfmpeg :: FfmpegToUse
  }

-- | This type describes how to obain a browser + browser driver combination.
data BrowserDependenciesSpec = BrowserDependenciesSpecChrome {
  browserDependenciesSpecChromeChrome :: ChromeToUse
  , browserDependenciesSpecChromeChromeDriver :: ChromeDriverToUse
  }
  | BrowserDependenciesSpecFirefox {
      browserDependenciesSpecFirefoxFirefox :: FirefoxToUse
      , browserDependenciesSpecFirefoxGeckodriver :: GeckoDriverToUse
      }

-- | This configuration will
--
-- * Use @java@ from the PATH, failing if it isn't found.
-- * Download Selenium to @\/tmp\/tools@, reusing the one there if found.
-- * Use @firefox@ from the PATH as the browser.
-- * Download a compatible @geckodriver@ to @\/tmp\/tools@, reusing the one there if found.
-- * If applicable, it will also get `xvfb-run`, `fluxbox`, and/or `ffmpeg` from the PATH.
--
-- But, it's easy to customize this behavior. You can define your own 'WebDriverDependencies' and customize
-- how each of these dependencies are found.
defaultWebDriverDependencies = WebDriverDependencies {
  webDriverDependencyJava = Nothing
  , webDriverDependencySelenium = DownloadSeleniumDefault "/tmp/tools"
  , webDriverDependencyBrowser = BrowserDependenciesSpecFirefox UseFirefoxFromPath (DownloadGeckoDriverAutodetect "/tmp/tools")
  , webDriverXvfb = XvfbDependenciesSpec UseXvfbFromPath (Just UseFluxboxFromPath)
  , webDriverFfmpeg = UseFfmpegFromPath
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

-- | Inroduce 'BrowserDependencies' via Nix, using the command line options.
-- This is useful to create the context for functions like 'allocateWebDriver'.
introduceBrowserDependenciesViaNix :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context, HasSomeCommandLineOptions context
  )
  -- | Child spec
  => SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceBrowserDependenciesViaNix = introduce "Introduce browser dependencies" browserDependencies alloc (const $ return ())
  where
    alloc = do
      SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})}) <- getSomeCommandLineOptions

      let useChrome = BrowserDependenciesChrome <$> getBinaryViaNixPackage @"google-chrome-stable" "google-chrome"
                                                <*> getBinaryViaNixPackage @"chromedriver" "chromedriver"

      let useFirefox = BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
                                                  <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"

      deps <- case optFirefox of
        Just UseChrome -> useChrome
        Just UseFirefox -> useFirefox
        Nothing -> useChrome

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
