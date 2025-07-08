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
  , introduceBrowserDependenciesViaNix'
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
  -- | How to obtain @Xvfb@ (used for the 'RunInXvfb' 'RunMode').
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
-- * If applicable, it will also get @Xvfb@, @fluxbox@, and/or @ffmpeg@ from the PATH.
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
  MonadUnliftIO m, MonadLogger m
  , MonadReader context m, HasBaseContext context, HasSomeCommandLineOptions context
  ) => BrowserDependenciesSpec -> m BrowserDependencies
getBrowserDependencies BrowserDependenciesSpecChrome {..} = do
  SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions{..})}) <- getSomeCommandLineOptions
  chrome <- maybe (exceptionOnLeft (obtainChrome browserDependenciesSpecChromeChrome)) pure optChromeBinary
  chromeDriver <- maybe (exceptionOnLeft (obtainChromeDriver browserDependenciesSpecChromeChromeDriver)) pure optChromeDriverBinary
  info [i|chrome: #{chrome}|]
  info [i|chromedriver: ''#{chromeDriver}|]
  return $ BrowserDependenciesChrome chrome chromeDriver
getBrowserDependencies (BrowserDependenciesSpecFirefox {..}) = do
  SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions{..})}) <- getSomeCommandLineOptions
  firefox <- maybe (exceptionOnLeft $ obtainFirefox browserDependenciesSpecFirefoxFirefox) pure optFirefoxBinary
  geckoDriver <- maybe (exceptionOnLeft $ obtainGeckoDriver browserDependenciesSpecFirefoxGeckodriver) pure optGeckoDriverBinary
  info [i|firefox: #{firefox}|]
  info [i|geckodriver: #{geckoDriver}|]
  return $ BrowserDependenciesFirefox firefox geckoDriver

-- | Introduce 'BrowserDependencies' via Nix, using the command line options.
-- This is useful to create the context for functions like 'allocateWebDriver'.
introduceBrowserDependenciesViaNix :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context, HasSomeCommandLineOptions context
  )
  -- | Child spec
  => SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceBrowserDependenciesViaNix = introduceBrowserDependenciesViaNix' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Same as 'introduceBrowserDependenciesViaNix', but allows passing custom 'NodeOptions'.
introduceBrowserDependenciesViaNix' :: forall m context. (
  MonadUnliftIO m, HasBaseContext context, HasNixContext context, HasSomeCommandLineOptions context
  )
  => NodeOptions
  -- | Child spec
  -> SpecFree (LabelValue "browserDependencies" BrowserDependencies :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceBrowserDependenciesViaNix' nodeOptions = introduce' nodeOptions "Introduce browser dependencies" browserDependencies alloc (const $ return ())
  where
    alloc = do
      SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})}) <- getSomeCommandLineOptions

      let useChrome = BrowserDependenciesChrome
            <$> maybe (getBinaryViaNixPackage @"google-chrome-stable" "google-chrome") pure optChromeBinary
            <*> maybe (getBinaryViaNixPackage @"chromedriver" "chromedriver") pure optChromeDriverBinary

      -- let useFirefox = case os of
      --       "darwin" -> do
      --         -- The only Firefox version that currently works on Darwin as of 5/5/2025 is firefox-bin
      --         firefox <- buildNixPackage "firefox-bin" >>= (liftIO . defaultFindFile "firefox")
      --         BrowserDependenciesFirefox firefox <$> getBinaryViaNixPackage @"geckodriver" "geckodriver"
      --       _ -> BrowserDependenciesFirefox <$> getBinaryViaNixPackage @"firefox" "firefox"
      --                                       <*> getBinaryViaNixPackage @"geckodriver" "geckodriver"

      let useFirefox = BrowserDependenciesFirefox
            <$> maybe (getBinaryViaNixPackage @"firefox" "firefox") pure optFirefoxBinary
            <*> maybe (getBinaryViaNixPackage @"geckodriver" "geckodriver") pure optGeckoDriverBinary

      deps <- case optBrowserToUse of
        Just UseChrome -> useChrome
        Just UseFirefox -> useFirefox
        Nothing -> useChrome

      info [i|Got browser dependencies: #{deps}|]

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
