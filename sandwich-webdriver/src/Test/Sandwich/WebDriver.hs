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
  ) where

import Control.Concurrent
import Control.Exception.Safe as ES
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
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

withBrowser1 :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m) => ExampleT (ContextWithSession context) W.WD a -> ExampleT context m a
withBrowser1 = withBrowser "browser1"

withBrowser2 :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m) => ExampleT (ContextWithSession context) W.WD a -> ExampleT context m a
withBrowser2 = withBrowser "browser2"

withBrowser :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m) => Browser -> ExampleT (ContextWithSession context) W.WD a -> ExampleT context m a
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

  let runAction action = do
        W.runWD sess $ do
          -- After the action, grab the updated session and save it before we return
          -- TODO: why is this necessary?
          ES.finally action $ do
            sess' <- W.getSession
            liftIO $ modifyMVar_ wdSessionMap $ return . M.insert browser sess'

  ExampleT (withReaderT (\ctx -> LabelValue ref :> ctx) $ mapReaderT (mapExceptT $ mapLoggingT $ (liftIO . runAction)) readerMonad)

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

instance W.WDSessionState m => W.WDSessionState (LoggingT m) where
  getSession = lift W.getSession
  putSession = lift . W.putSession

instance W.WebDriver wd => W.WebDriver (LoggingT wd) where
  doCommand rm t a = lift (W.doCommand rm t a)

instance (W.WDSessionState (ExampleT context wd), W.WebDriver wd) => W.WebDriver (ExampleT context wd) where
  doCommand rm t a = lift (W.doCommand rm t a)

type HasWebDriverContext context = HasLabel context "webdriver" WdSession
type HasWebDriverSessionContext context = HasLabel context "webdriverSession" (IORef W.WDSession)
type ExampleWithWebDriver context wd = (W.WDSessionState (ExampleT context wd), W.WebDriver wd)
