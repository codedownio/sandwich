{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.WebDriver (
  -- * Introducing a WebDriver server
  introduceWebDriver
  , introduceWebDriverViaNix

  -- * Specifying how to obtain dependencies
  , defaultWebDriverDependencies
  , WebDriverDependencies(..)

  -- * Running an example in a given session
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
  , introduceWebDriver'
  , addCommandLineOptionsToWdOptions

  -- * Context types
  , webdriver
  , HasWebDriverContext
  , webdriverSession
  , HasWebDriverSessionContext

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Config
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W
import UnliftIO.MVar


-- | Introduce a 'WebDriver', using the given 'WebDriverDependencies'.
-- A good default is 'defaultWebDriverDependencies'.
introduceWebDriver :: forall context m. (
  BaseMonadContext m context, HasSomeCommandLineOptions context
  )
  -- | How to obtain dependencies
  => WebDriverDependencies
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriver wdd wdOptions = introduceWebDriver' wdd alloc wdOptions
  where
    alloc wdOptions' = do
      clo <- getSomeCommandLineOptions
      allocateWebDriver (addCommandLineOptionsToWdOptions clo wdOptions')

-- | Introduce a 'WebDriver' using the current 'NixContext'.
-- This will pull everything required from the configured Nixpkgs snapshot.
introduceWebDriverViaNix :: forall m context. (
  BaseMonadContext m context, HasSomeCommandLineOptions context, HasNixContext context
  )
  -- | Options
  => WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m ()
  -> SpecFree context m ()
introduceWebDriverViaNix wdOptions =
  introduceFileViaNixPackage' @"selenium.jar" "selenium-server-standalone" (findFirstFile (return . (".jar" `L.isSuffixOf`)))
  . introduceBinaryViaNixPackage @"java" "jre"
  . introduceBrowserDependenciesViaNix
  . introduce "Introduce WebDriver session" webdriver alloc cleanupWebDriver
  where
    alloc = do
      clo <- getSomeCommandLineOptions
      allocateWebDriver (addCommandLineOptionsToWdOptions clo wdOptions)

-- | Same as 'introduceWebDriver', but with a controllable allocation callback.
introduceWebDriver' :: forall m context. (
  BaseMonadContext m context
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
  BaseMonad m
  , HasBaseContext context, HasFile context "java", HasFile context "selenium.jar", HasBrowserDependencies context
  )
  -- | Options
  => WdOptions
  -> ExampleT context m WebDriver
allocateWebDriver wdOptions = do
  dir <- fromMaybe "/tmp" <$> getCurrentFolder
  startWebDriver wdOptions dir

-- | Clean up the given WebDriver.
cleanupWebDriver :: (BaseMonad m) => WebDriver -> ExampleT context m ()
cleanupWebDriver sess = do
  closeAllSessions sess
  stopWebDriver sess

-- | Run a given example using a given Selenium session.
withSession :: forall m context a. (
  WebDriverMonad m context
  )
  -- | Session to run
  => Session
  -> ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a
  -> ExampleT context m a
withSession session (ExampleT readerMonad) = do
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
  let f :: m a -> m a = id

  ExampleT (withReaderT (\ctx -> LabelValue (session, ref) :> ctx) $ mapReaderT (mapLoggingT f) readerMonad)

-- | Convenience function. 'withSession1' = 'withSession' "session1".
withSession1 :: WebDriverMonad m context => ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a -> ExampleT context m a
withSession1 = withSession "session1"

-- | Convenience function. 'withSession2' = 'withSession' "session2".
withSession2 :: WebDriverMonad m context => ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) m a -> ExampleT context m a
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
