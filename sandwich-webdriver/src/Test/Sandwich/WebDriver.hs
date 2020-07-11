{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module Test.Sandwich.WebDriver (
  webdriver
  , introduceWebdriver
  , allocateWebDriver
  , allocateWebDriver'
  , cleanupWebDriver
  , cleanupWebDriver'

  , withBrowser1
  , withBrowser2
  , withBrowser
  , closeAllSessions
  , getBrowsers
  , Browser

  , RunMode(..)

  , chromeCapabilities
  , headlessChromeCapabilities

  , WdSession
  , getDisplayNumber

  , ExampleWithWebDriver
  , HasWebDriverContext
  , HasWebDriverSessionContext
  , ContextWithSession

  , WdOptions
  , SeleniumToUse(..)
  , ChromeDriverToUse(..)
  , obtainSelenium
  , obtainChromeDriver
  , defaultWdOptions
  , runMode
  , saveSeleniumMessageHistory
  , seleniumToUse
  , chromeDriverToUse
  , WhenToSave(..)
  , capabilities

  , hoistExample -- experimental
  ) where

import Control.Concurrent
import Control.Exception.Safe as ES
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Internal as WI
import qualified Test.WebDriver.Session as W


introduceWebdriver :: (HasBaseContext context, MonadIO m, MonadCatch m, MonadBaseControl IO m) => WdOptions -> SpecFree (LabelValue "webdriver" WdSession :> context) m () -> SpecFree context m ()
introduceWebdriver wdOptions = introduce "Introduce WebDriver session" webdriver (allocateWebDriver wdOptions) cleanupWebDriver

allocateWebDriver :: (HasBaseContext context, MonadIO m, MonadBaseControl IO m) => WdOptions -> ExampleT context m WdSession
allocateWebDriver wdOptions = do
  debug "Beginning allocateWebDriver"
  maybeRunRoot <- getRunRoot
  let runRoot = fromMaybe "/tmp" maybeRunRoot
  startWebDriver wdOptions runRoot

allocateWebDriver' :: FilePath -> WdOptions -> IO WdSession
allocateWebDriver' runRoot wdOptions = do
  runNoLoggingT $ startWebDriver wdOptions runRoot

cleanupWebDriver :: (HasBaseContext context, MonadIO m, MonadCatch m, MonadBaseControl IO m) => WdSession -> ExampleT context m ()
cleanupWebDriver sess = do
  debug "Doing cleanupWebDriver"
  closeAllSessions sess
  stopWebDriver sess
  debug "Finished cleanupWebDriver"

cleanupWebDriver' :: WdSession -> IO ()
cleanupWebDriver' sess = do
  runNoLoggingT $ do
    closeAllSessions sess
    stopWebDriver sess

withBrowser1 :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m) => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withBrowser1 = withBrowser "browser1"

withBrowser2 :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m) => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withBrowser2 = withBrowser "browser2"

withBrowser :: forall m a context. (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m) => Browser -> ExampleT (ContextWithSession context) m a -> ExampleT context m a
withBrowser browser (ExampleT readerMonad) = do
  WdSession {..} <- getContext webdriver
  -- Create new session if necessary (this can throw an exception)
  sess <- liftIO $ modifyMVar wdSessionMap $ \sessionMap -> case M.lookup browser sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      sess'' <- W.mkSession wdConfig
      let sess' = sess'' { W.wdSessHistUpdate = W.unlimitedHistory }
      sess <- W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
      return (M.insert browser sess sessionMap, sess)

  ref <- liftIO $ newIORef sess

  -- Not used for now, but previous libraries have use a finally to grab the final session on exception.
  -- We could do the same here, but it's not clear that it's needed.
  let f :: m a -> m a = id

  ExampleT (withReaderT (\ctx -> LabelValue ref :> ctx) $ mapReaderT (mapLoggingT f) readerMonad)

hoistExample :: ExampleT context IO a -> ExampleT (ContextWithSession context) IO a
hoistExample (ExampleT r) = ExampleT $ transformContext r
  where transformContext = withReaderT (\(_ :> ctx) -> ctx)

getBrowsers :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m, MonadReader context m) => m [Browser]
getBrowsers = do
  WdSession {..} <- getContext webdriver
  M.keys <$> liftIO (readMVar wdSessionMap)

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

type HasWebDriverContext context = HasLabel context "webdriver" WdSession
type HasWebDriverSessionContext context = HasLabel context "webdriverSession" (IORef W.WDSession)
type ExampleWithWebDriver context wd = (W.WDSessionState (ExampleT context wd), W.WebDriver wd)
