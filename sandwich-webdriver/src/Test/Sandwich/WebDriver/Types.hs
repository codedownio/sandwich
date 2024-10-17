{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sandwich.WebDriver.Types (
  -- * Type aliases to make signatures shorter
  BaseMonad
  , ContextWithWebdriverDeps
  , ContextWithBaseDeps
  , WebDriverMonad

  -- * Context aliases
  , HasBrowserDependencies
  , HasWebDriverContext
  , HasWebDriverSessionContext

  -- * The Xvfb session
  , XvfbSession(..)
  , getXvfbSession

  -- * Misc helpers
  , hoistExample
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Internal as WI
import qualified Test.WebDriver.Session as W
import UnliftIO.Exception as ES


instance (MonadIO m, HasWebDriverSessionContext context) => W.WDSessionState (ExampleT context m) where
  getSession = do
    (_, sessVar) <- getContext webdriverSession
    liftIO $ readIORef sessVar
  putSession sess = do
    (_, sessVar) <- getContext webdriverSession
    liftIO $ writeIORef sessVar sess

-- Implementation copied from that of the WD monad implementation
instance (MonadIO m, MonadBaseControl IO m, HasWebDriverSessionContext context) => W.WebDriver (ExampleT context m) where
  doCommand method path args = WI.mkRequest method path args
    >>= WI.sendHTTPRequest
    >>= either throwIO return
    >>= WI.getJSONResult
    >>= either throwIO return

type HasWebDriverContext context = HasLabel context "webdriver" WebDriver
type HasWebDriverSessionContext context = HasLabel context "webdriverSession" WebDriverSession

type ContextWithWebdriverDeps context =
  LabelValue "webdriver" WebDriver
  :> ContextWithBaseDeps context

type ContextWithBaseDeps context =
  -- | Browser dependencies
  LabelValue "browserDependencies" BrowserDependencies
  -- | Java
  :> LabelValue "file-java" (EnvironmentFile "java")
  -- | Selenium
  :> LabelValue "file-selenium.jar" (EnvironmentFile "selenium.jar")
  -- | Base context
  :> context

hoistExample :: ExampleT context IO a -> ExampleT (LabelValue "webdriverSession" WebDriverSession :> context) IO a
hoistExample (ExampleT r) = ExampleT $ transformContext r
  where transformContext = withReaderT (\(_ :> ctx) -> ctx)

type BaseMonad m context = (HasCallStack, MonadUnliftIO m, MonadMask m, HasBaseContext context)
type WebDriverMonad m context = (HasCallStack, MonadUnliftIO m, HasWebDriverContext context)
