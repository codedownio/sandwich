{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sandwich.WebDriver.Types (
  ExampleWithWebDriver
  , HasWebDriverContext
  , HasWebDriverSessionContext
  , ContextWithSession

  , hoistExample

  , webdriver

  -- * Constraint synonyms
  , BaseMonad
  , BaseMonadContext
  , WebDriverMonad
  , WebDriverSessionMonad
  ) where

import Control.Exception.Safe as ES
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Internal as WI
import qualified Test.WebDriver.Session as W


type ContextWithSession context = LabelValue "webdriverSession" (IORef W.WDSession) :> context

instance (MonadIO m, HasLabel context "webdriverSession" (IORef W.WDSession)) => W.WDSessionState (ExampleT context m) where
  getSession = do
    sessVar <- getContext webdriverSession
    liftIO $ readIORef sessVar
  putSession sess = do
    sessVar <- getContext webdriverSession
    liftIO $ writeIORef sessVar sess

-- Implementation copied from that of the WD monad implementation
instance (MonadIO m, MonadThrow m, HasLabel context "webdriverSession" (IORef W.WDSession), MonadBaseControl IO m) => W.WebDriver (ExampleT context m) where
  doCommand method path args = WI.mkRequest method path args
    >>= WI.sendHTTPRequest
    >>= either throwIO return
    >>= WI.getJSONResult
    >>= either throwIO return

type HasWebDriverContext context = HasLabel context "webdriver" WebDriver
type HasWebDriverSessionContext context = HasLabel context "webdriverSession" (IORef W.WDSession)
type ExampleWithWebDriver context wd = (W.WDSessionState (ExampleT context wd), W.WebDriver wd)

hoistExample :: ExampleT context IO a -> ExampleT (ContextWithSession context) IO a
hoistExample (ExampleT r) = ExampleT $ transformContext r
  where transformContext = withReaderT (\(_ :> ctx) -> ctx)

type WebDriverMonad m context = (HasCallStack, HasLabel context "webdriver" WebDriver, MonadIO m, MonadBaseControl IO m)
type WebDriverSessionMonad m context = (WebDriverMonad m context, MonadReader context m, HasLabel context "webdriver" WebDriver)
type BaseMonad m = (HasCallStack, MonadIO m, MonadCatch m, MonadBaseControl IO m, MonadMask m)
type BaseMonadContext m context = (BaseMonad m, HasBaseContext context)
