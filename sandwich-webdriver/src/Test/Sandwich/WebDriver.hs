{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.WebDriver (
  -- * Introducing a WebDriver server
  introduceWebDriver
  , introduceWebDriverViaNix
  , introduceWebDriverOptions
  , introduceWebDriver'
  , addCommandLineOptionsToWdOptions

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
  , introduceBrowserDependenciesViaNix

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Class
  , module Test.Sandwich.WebDriver.Config
  , module Test.Sandwich.WebDriver.Types
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Class
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.BrowserDependencies
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W
import UnliftIO.MVar
import UnliftIO.Process


-- | This is the main 'introduce' method for creating a WebDriver.
introduceWebDriver :: (
  BaseMonadContext m context
  ) => WdOptions -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriver = introduceWebDriver' defaultWebDriverDependencies return

introduceWebDriverViaNix :: forall m context. (
  BaseMonadContext m context, HasSomeCommandLineOptions context, HasNixContext context
  ) => WdOptions -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriverViaNix wdOptions =
  introduceFileViaNixPackage @"selenium.jar" "selenium-server-standalone" tryFindSeleniumJar
  . introduceBinaryViaNixPackage @"java" "jre"
  . introduceBrowserDependenciesViaNix
  . (introduceWith "Introduce WebDriver session" webdriver $ \action -> do
      runRoot <- fromMaybe "/tmp" <$> getCurrentFolder
      clo <- getSomeCommandLineOptions
      void $ withWebDriver (addCommandLineOptionsToWdOptions clo wdOptions) runRoot action
    )
  where
    tryFindSeleniumJar :: FilePath -> IO FilePath
    tryFindSeleniumJar path = (T.unpack . T.strip . T.pack) <$> readCreateProcess (proc "find" [path, "-name", "*.jar"]) ""

-- | Same as introduceWebDriver, but merges command line options into the 'WdOptions'.
introduceWebDriverOptions :: forall context m. (
  BaseMonadContext m context, HasSomeCommandLineOptions context
  )
  -- | Dependencies
  => WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriverOptions wdOptions = introduceWebDriverOptions' defaultWebDriverDependencies wdOptions

-- | Same as 'introduceWebDriverOptions', but allows you to customize the 'WebDriverDependencies'.
introduceWebDriverOptions' :: forall context m. (
  BaseMonadContext m context, HasSomeCommandLineOptions context
  )
  -- | Dependencies
  => WebDriverDependencies
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriverOptions' wdd wdOptions = introduceWebDriver' wdd alloc wdOptions
  where
    alloc wdOptions' = do
      clo <- getSomeCommandLineOptions
      return $ addCommandLineOptionsToWdOptions clo wdOptions'

introduceWebDriver' :: forall m context. (
  BaseMonadContext m context
  )
  -- | Dependencies
  => WebDriverDependencies
  -> (WdOptions -> ExampleT (ContextWithBaseDeps context) m WdOptions)
  -> WdOptions
  -> SpecFree (ContextWithWebdriverDeps context) m () -> SpecFree context m ()
introduceWebDriver' (WebDriverDependencies {..}) transformWdOptions wdOptions =
  introduce "Introduce selenium.jar" (mkFileLabel @"selenium.jar") ((EnvironmentFile <$>) $ obtainSelenium webDriverDependencySelenium) (const $ return ())
  . (case webDriverDependencyJava of Nothing -> introduceBinaryViaEnvironment @"java"; Just p -> introduceFile @"java" p)
  . introduce "Introduce browser dependencies" browserDependencies (getBrowserDependencies webDriverDependencyBrowser) (const $ return ())
  . (introduceWith "Introduce WebDriver session" webdriver $ \action -> do
      runRoot <- fromMaybe "/tmp" <$> getCurrentFolder
      wdOptions' <- transformWdOptions wdOptions
      void $ withWebDriver wdOptions' runRoot action
    )

type ContextWithWebdriverDeps context =
  LabelValue "webdriver" WebDriver
  :> ContextWithBaseDeps context

type ContextWithBaseDeps context =
  LabelValue "browserDependencies" BrowserDependencies
  :> LabelValue "file-java" (EnvironmentFile "java")
  :> LabelValue "file-selenium.jar" (EnvironmentFile "selenium.jar")
  :> context

-- | Run a given example using a given Selenium session.
withSession :: forall m context a. (
  WebDriverMonad m context
  ) => Session -> ExampleT (ContextWithSession context) m a -> ExampleT context m a
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

-- | Convenience function. 'withSession1' = 'withSession' "session1"
withSession1 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession1 = withSession "session1"

-- | Convenience function. 'withSession2' = 'withSession' "session2"
withSession2 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession2 = withSession "session2"

-- | Get all existing session names
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
