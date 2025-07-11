{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Introduce [WebDriver](https://www.selenium.dev/documentation/webdriver/) servers and sessions.
-}

module Test.Sandwich.WebDriver (
  -- * Introducing a WebDriver server
  introduceWebDriver
  , introduceWebDriverViaNix
  , introduceWebDriverViaNix'

  -- * Non-Nix dependency fetching
  -- | When you aren't using Nix, these types specify how to obtain the necessary dependencies.
  , defaultWebDriverDependencies
  , WebDriverDependencies(..)

  -- * Running an example in a given session
  -- | Once you have a 'WebDriver' in context, you can run one or more sessions.
  -- Each session will open an independent browser instance.
  , withSession
  , withSession1
  , withSession2

  -- * Managing sessions
  , getSessions
  , closeCurrentSession
  , closeSession
  , closeAllSessions
  , closeAllSessionsExcept
  , SessionName

  -- * Lower-level allocation functions
  , allocateWebDriver
  , cleanupWebDriver
  , introduceBrowserDependenciesViaNix
  , introduceBrowserDependenciesViaNix'
  , introduceWebDriver'
  , addCommandLineOptionsToWdOptions

  -- * Context types
  -- ** WebDriver
  , webdriver
  , TestWebDriverContext
  , HasTestWebDriverContext
  -- ** WebDriverSession
  , webdriverSession
  , WebDriverSession
  , HasWebDriverSessionContext

  -- * Shorthands
  -- | These are used to make type signatures shorter.
  , BaseMonad
  , ContextWithBaseDeps
  , ContextWithWebdriverDeps
  , WebDriverMonad
  , WebDriverSessionMonad

  -- * On demand options
  , OnDemandOptions
  , defaultOnDemandOptions

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Config
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Lens.Micro
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver.Binaries
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Capabilities.Extra
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util (makeUUID)
import Test.Sandwich.WebDriver.Types
import Test.Sandwich.WebDriver.Video (recordVideoIfConfigured)
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import UnliftIO.Directory
import UnliftIO.Exception (bracket)
import UnliftIO.MVar


-- | Introduce a 'WebDriver', using the given 'WebDriverDependencies'.
-- A good default is 'defaultWebDriverDependencies'.
introduceWebDriver :: forall context m. (
  BaseMonad m context, HasSomeCommandLineOptions context
  )
  -- | How to obtain dependencies
  => WebDriverDependencies
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriver wdd wdOptions = introduceWebDriver' wdd alloc wdOptions
  where
    alloc wdOptions' = do
      clo <- getSomeCommandLineOptions
      allocateWebDriver (addCommandLineOptionsToWdOptions clo wdOptions') onDemandOptions

    onDemandOptions = OnDemandOptions {
      ffmpegToUse = webDriverFfmpeg wdd
      , xvfbToUse = xvfbDependenciesSpecXvfb $ webDriverXvfb wdd
      }

-- | Introduce a 'WebDriver' using the current 'NixContext'.
-- This will pull everything required from the configured Nixpkgs snapshot.
introduceWebDriverViaNix :: forall m context. (
  BaseMonad m context, HasSomeCommandLineOptions context, HasNixContext context
  )
  -- | Options
  => WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m ()
  -> SpecFree context m ()
introduceWebDriverViaNix = introduceWebDriverViaNix' (defaultNodeOptions { nodeOptionsVisibilityThreshold = defaultFileContextVisibilityThreshold })

-- | Same as 'introduceWebDriverViaNix', but allows passing custom 'NodeOptions'.
introduceWebDriverViaNix' :: forall m context. (
  BaseMonad m context, HasSomeCommandLineOptions context, HasNixContext context
  )
  => NodeOptions
  -- | Options
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m ()
  -> SpecFree context m ()
introduceWebDriverViaNix' nodeOptions wdOptions =
  introduceBrowserDependenciesViaNix' nodeOptions
  -- We use 'introduceWith' here because the alloc function will start an Async to read the logs from the Selenium
  -- process and log them. If we were to use 'introduce', the test_logs.txt file would be closed after the allocate section
  -- and the write handle would become invalid.
  . introduceWith "Introduce WebDriver session" webdriver (\action -> bracket alloc cleanupWebDriver (void . action))
  where
    alloc = do
      clo <- getSomeCommandLineOptions

      nc <- getContext nixContext
      let onDemandOptions = OnDemandOptions {
            ffmpegToUse = UseFfmpegFromNixpkgs nc
            , xvfbToUse = UseXvfbFromNixpkgs nc
            }

      allocateWebDriver (addCommandLineOptionsToWdOptions clo wdOptions) onDemandOptions

-- | Same as 'introduceWebDriver', but with a controllable allocation callback.
introduceWebDriver' :: forall m context. (
  BaseMonad m context, HasSomeCommandLineOptions context
  )
  -- | Dependencies
  => WebDriverDependencies
  -> (WdOptions -> ExampleT (ContextWithBaseDeps context) m TestWebDriverContext)
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriver' (WebDriverDependencies {..}) alloc wdOptions =
  introduce "Introduce browser dependencies" browserDependencies (getBrowserDependencies webDriverDependencyBrowser) (const $ return ())
  -- We use 'introduceWith' deliberately here instead of 'introduce'. See comment on above.
  . introduceWith "Introduce WebDriver session" webdriver (\action -> bracket (alloc wdOptions) cleanupWebDriver (void . action))

-- | Allocate a WebDriver using the given options.
allocateWebDriver :: (
  BaseMonad m context, HasBrowserDependencies context
  )
  -- | Options
  => WdOptions
  -> OnDemandOptions
  -> ExampleT context m TestWebDriverContext
allocateWebDriver wdOptions (OnDemandOptions {..}) = do
  runRoot <- fromMaybe "/tmp" <$> getCurrentFolder

  driverConfig <- getContext browserDependencies >>= \case
    BrowserDependenciesChrome {..} -> return $ W.DriverConfigChromedriver {
      driverConfigChromedriver = browserDependenciesChromeChromedriver
      , driverConfigChrome = browserDependenciesChromeChrome
      , driverConfigLogDir = runRoot
      , driverConfigChromedriverFlags = chromedriverExtraFlags wdOptions
      }
    BrowserDependenciesFirefox {..} -> return $ W.DriverConfigGeckodriver {
      driverConfigGeckodriver = browserDependenciesFirefoxGeckodriver
      , driverConfigFirefox = browserDependenciesFirefoxFirefox
      , driverConfigLogDir = runRoot
      , driverConfigGeckodriverFlags = geckodriverExtraFlags wdOptions
      }

  -- Create a unique name for this webdriver so the folder for its log output doesn't conflict with any others
  webdriverName <- ("webdriver_" <>) <$> liftIO makeUUID

  -- Directory to log everything for this webdriver
  let webdriverRoot = runRoot </> (T.unpack webdriverName)
  liftIO $ createDirectoryIfMissing True webdriverRoot

  -- Directory to hold any downloads
  let downloadDir = webdriverRoot </> "Downloads"
  liftIO $ createDirectoryIfMissing True downloadDir

  -- Final extra capabilities configuration
  finalCaps <-
    getContext browserDependencies
    >>= getCapabilitiesForBrowser
    >>= configureChromeNoSandbox wdOptions
    >>= configureHeadlessChromeCapabilities wdOptions (runMode wdOptions)
    >>= configureHeadlessFirefoxCapabilities wdOptions (runMode wdOptions)
    >>= configureChromeDownloadCapabilities downloadDir
    >>= configureFirefoxDownloadCapabilities downloadDir

  wdc <- W.mkEmptyWebDriverContext

  TestWebDriverContext
    <$> pure (T.unpack webdriverName)
    <*> pure wdc
    <*> pure (wdOptions { capabilities = finalCaps })
    <*> liftIO (newMVar mempty)
    <*> pure driverConfig
    <*> pure downloadDir

    <*> pure ffmpegToUse
    <*> newMVar OnDemandNotStarted

    <*> pure xvfbToUse
    <*> newMVar OnDemandNotStarted


-- | Clean up the given WebDriver.
cleanupWebDriver :: (BaseMonad m context) => TestWebDriverContext -> ExampleT context m ()
cleanupWebDriver sess = do
  closeAllSessions sess
  stopWebDriver sess

-- | Run a given example using a given Selenium session.
withSession :: forall m context a. (
  MonadMask m
  , HasBaseContext context, HasSomeCommandLineOptions context, WebDriverMonad m context
  )
  -- | Session to run
  => SessionName
  -> ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a
  -> ExampleT context m a
withSession sessionName action = do
  TestWebDriverContext {..} <- getContext webdriver

  -- Create new session if necessary (this can throw an exception)
  sess <- modifyMVar wdSessionMap $ \sessionMap -> case M.lookup sessionName sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      finalCaps <- pure (capabilities wdOptions)
        >>= configureChromeUserDataDir

      debug [i|Creating session '#{sessionName}'|]
      sess <- W.startSession wdContext wdDriverConfig finalCaps sessionName
      return (M.insert sessionName sess sessionMap, sess)

  pushContext webdriverSession (sessionName, sess) $
    recordVideoIfConfigured sessionName
    action

-- | Convenience function. @withSession1 = withSession "session1"@.
withSession1 :: (
  MonadMask m
  , HasBaseContext context, HasSomeCommandLineOptions context, WebDriverMonad m context
  )
  -- | Wrapped action
  => ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a
  -> ExampleT context m a
withSession1 = withSession "session1"

-- | Convenience function. @withSession2 = withSession "session2"@.
withSession2 :: (
  MonadMask m
  , HasBaseContext context, HasSomeCommandLineOptions context, WebDriverMonad m context
  )
  -- | Wrapped action
  => ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a
  -> ExampleT context m a
withSession2 = withSession "session2"

-- | Get all existing session names.
getSessions :: (MonadReader context m, WebDriverMonad m context) => m [SessionName]
getSessions = do
  TestWebDriverContext {..} <- getContext webdriver
  M.keys <$> liftIO (readMVar wdSessionMap)

-- | Merge the options from the 'CommandLineOptions' into some 'WdOptions'.
addCommandLineOptionsToWdOptions :: SomeCommandLineOptions -> WdOptions -> WdOptions
addCommandLineOptionsToWdOptions (SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})})) wdOptions@(WdOptions {..}) = wdOptions {
  runMode = case optDisplay of
    Nothing -> runMode
    Just Headless -> RunHeadless defaultHeadlessConfig
    Just Xvfb -> RunInXvfb (defaultXvfbConfig { xvfbStartFluxbox = optFluxbox })
    Just Current -> Normal
  , chromeNoSandbox = optChromeNoSandbox
  }

getCapabilitiesForBrowser :: MonadIO m => BrowserDependencies -> m W.Capabilities
getCapabilitiesForBrowser (BrowserDependenciesChrome {..}) = pure $ W.defaultCaps {
  W._capabilitiesBrowserName = Just "chrome"
  , W._capabilitiesGoogChromeOptions = Just $
    W.defaultChromeOptions
      & set W.chromeOptionsBinary (Just browserDependenciesChromeChrome)
  }
getCapabilitiesForBrowser (BrowserDependenciesFirefox {..}) = pure $ W.defaultCaps {
  W._capabilitiesBrowserName = Just "firefox"
  , W._capabilitiesMozFirefoxOptions = Just $
    W.defaultFirefoxOptions
      & set W.firefoxOptionsBinary (Just browserDependenciesFirefoxFirefox)
  }
