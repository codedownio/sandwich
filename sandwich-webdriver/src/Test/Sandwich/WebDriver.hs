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
  , Session

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
  , WebDriver
  , HasWebDriverContext
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

  -- * On demand options
  , OnDemandOptions
  , defaultOnDemandOptions

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Config
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver.Binaries
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import Test.Sandwich.WebDriver.Video (recordVideoIfConfigured)
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W
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
  introduceFileViaNixPackage'' @"selenium.jar" nodeOptions "selenium-server-standalone" (findFirstFile (return . (".jar" `L.isSuffixOf`)))
  . introduceBinaryViaNixPackage' @"java" nodeOptions "jre"
  . introduceBrowserDependenciesViaNix' nodeOptions
  . introduce "Introduce WebDriver session" webdriver alloc cleanupWebDriver
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
  BaseMonad m context
  )
  -- | Dependencies
  => WebDriverDependencies
  -> (WdOptions -> ExampleT (ContextWithBaseDeps context) m WebDriver)
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriver' (WebDriverDependencies {..}) alloc wdOptions =
  introduce "Introduce selenium.jar" (mkFileLabel @"selenium.jar") ((EnvironmentFile <$>) $ obtainSelenium webDriverDependencySelenium) (const $ return ())
  . (case webDriverDependencyJava of Nothing -> introduceBinaryViaEnvironment @"java"; Just p -> introduceFile @"java" p)
  . introduce "Introduce browser dependencies" browserDependencies (getBrowserDependencies webDriverDependencyBrowser) (const $ return ())
  . introduce "Introduce WebDriver session" webdriver (alloc wdOptions) cleanupWebDriver

-- | Allocate a WebDriver using the given options.
allocateWebDriver :: (
  BaseMonad m context
  , HasFile context "java", HasFile context "selenium.jar", HasBrowserDependencies context
  )
  -- | Options
  => WdOptions
  -> OnDemandOptions
  -> ExampleT context m WebDriver
allocateWebDriver wdOptions onDemandOptions = do
  dir <- fromMaybe "/tmp" <$> getCurrentFolder
  startWebDriver wdOptions onDemandOptions dir

-- | Clean up the given WebDriver.
cleanupWebDriver :: (BaseMonad m context) => WebDriver -> ExampleT context m ()
cleanupWebDriver sess = do
  closeAllSessions sess
  stopWebDriver sess

-- | Run a given example using a given Selenium session.
withSession :: forall m context a. (
  MonadMask m, MonadBaseControl IO m
  , HasBaseContext context, HasSomeCommandLineOptions context, WebDriverMonad m context
  )
  -- | Session to run
  => Session
  -> ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a
  -> ExampleT context m a
withSession session action = do
  WebDriver {..} <- getContext webdriver
  -- Create new session if necessary (this can throw an exception)
  sess <- modifyMVar wdSessionMap $ \sessionMap -> case M.lookup session sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      debug [i|Creating session '#{session}'|]
      sess'' <- liftIO $ W.mkSession wdConfig
      let sess' = sess'' { W.wdSessHistUpdate = W.unlimitedHistory }
      sess <- liftIO $ W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
      return (M.insert session sess sessionMap, sess)

  ref <- liftIO $ newIORef sess

  -- Not used for now, but previous libraries have use a finally to grab the final session on exception.
  -- We could do the same here, but it's not clear that it's needed.
  -- let f :: m a -> m a = id

  pushContext webdriverSession (session, ref) $
    recordVideoIfConfigured session action

-- | Convenience function. @withSession1 = withSession "session1"@.
withSession1 :: (
  MonadMask m, MonadBaseControl IO m
  , HasBaseContext context, HasSomeCommandLineOptions context, WebDriverMonad m context
  ) => ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a -> ExampleT context m a
withSession1 = withSession "session1"

-- | Convenience function. @withSession2 = withSession "session2"@.
withSession2 :: (
  MonadMask m, MonadBaseControl IO m
  , HasBaseContext context, HasSomeCommandLineOptions context, WebDriverMonad m context
  ) => ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a -> ExampleT context m a
withSession2 = withSession "session2"

-- | Get all existing session names.
getSessions :: (MonadReader context m, WebDriverMonad m context) => m [Session]
getSessions = do
  WebDriver {..} <- getContext webdriver
  M.keys <$> liftIO (readMVar wdSessionMap)

-- | Merge the options from the 'CommandLineOptions' into some 'WdOptions'.
addCommandLineOptionsToWdOptions :: SomeCommandLineOptions -> WdOptions -> WdOptions
addCommandLineOptionsToWdOptions (SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})})) wdOptions@(WdOptions {..}) = wdOptions {
  runMode = case optDisplay of
    Nothing -> runMode
    Just Headless -> RunHeadless defaultHeadlessConfig
    Just Xvfb -> RunInXvfb (defaultXvfbConfig { xvfbStartFluxbox = optFluxbox })
    Just Current -> Normal
  }
