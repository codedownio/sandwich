{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, TypeOperators, LambdaCase, DataKinds, UndecidableInstances, MultiParamTypeClasses, RecordWildCards, FlexibleContexts #-}

-- |

module Test.Sandwich.WebDriver (
  webdriver
  , introduceWebdriver
  , allocateWebDriver
  , cleanupWebDriver

  , withBrowser1

  , RunMode(..)

  , chromeCapabilities
  , headlessChromeCapabilities

  , WdOptions
  , defaultWdOptions
  , runMode
  , saveSeleniumMessageHistory
  , WhenToSave(..)
  , capabilities
  ) where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Capabilities
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W


webdriver = Label :: Label "webdriver" WdSession
webdriverSession = Label :: Label "webdriverSession" (IORef W.WDSession)

introduceWebdriver :: (HasBaseContext context) => WdOptions -> Spec (LabelValue "webdriver" WdSession :> context) () -> Spec context ()
introduceWebdriver wdOptions = introduce "Introduce WebDriver session" webdriver (allocateWebDriver wdOptions) cleanupWebDriver

allocateWebDriver wdOptions = do
  maybeRunRoot <- getRunRoot
  let runRoot = fromMaybe "/tmp" maybeRunRoot
  liftIO $ startWebDriver wdOptions runRoot

cleanupWebDriver :: (HasBaseContext context) => ExampleM (LabelValue "webdriver" WdSession :> context) ()
cleanupWebDriver = do
  session <- getContext webdriver
  liftIO $ closeAllSessions session
  liftIO $ stopWebDriver session

withBrowser1 :: HasLabel context "webdriver" WdSession => ExampleT (ContextWithSession context) W.WD a -> ExampleT context IO a
withBrowser1 = withBrowser "browser1"

withBrowser :: HasLabel context "webdriver" WdSession => Browser -> ExampleT (ContextWithSession context) W.WD a -> ExampleT context IO a
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

  ExampleT (withReaderT (\ctx -> LabelValue ref :> ctx) $ mapReaderT (mapExceptT $ mapLoggingT $ W.runWD sess) readerMonad)

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

