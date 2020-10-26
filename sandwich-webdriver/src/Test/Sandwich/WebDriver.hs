{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- |

module Test.Sandwich.WebDriver (
  -- * Introducing a WebDriver server
  introduceWebdriver

  -- * Running an example in a given session
  , withBrowser1
  , withBrowser2
  , withBrowser
  , closeAllSessions
  , getBrowsers
  , Browser

  -- * Lower-level allocation functions
  , allocateWebDriver
  , allocateWebDriver'
  , cleanupWebDriver
  , cleanupWebDriver'

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Config
  , module Test.Sandwich.WebDriver.Session
  , module Test.Sandwich.WebDriver.Types
  ) where

import Control.Concurrent.MVar.Lifted
import Control.Exception.Safe as ES
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Session
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W


-- | This is the main 'introduce' method for creating a WebDriver.
introduceWebdriver :: (HasBaseContext context, MonadIO m, MonadCatch m, MonadBaseControl IO m, MonadMask m) => WdOptions -> SpecFree (LabelValue "webdriver" WdSession :> context) m () -> SpecFree context m ()
introduceWebdriver wdOptions = introduce "Introduce WebDriver session" webdriver (allocateWebDriver wdOptions) cleanupWebDriver

allocateWebDriver :: (HasBaseContext context, MonadIO m, MonadBaseControl IO m, MonadMask m) => WdOptions -> ExampleT context m WdSession
allocateWebDriver wdOptions = do
  debug "Beginning allocateWebDriver"
  maybeRunRoot <- getRunRoot
  let runRoot = fromMaybe "/tmp" maybeRunRoot
  startWebDriver wdOptions runRoot

allocateWebDriver' :: FilePath -> WdOptions -> IO WdSession
allocateWebDriver' runRoot wdOptions = do
  runNoLoggingT $ startWebDriver wdOptions runRoot

cleanupWebDriver :: (HasBaseContext context, BaseMonad m) => WdSession -> ExampleT context m ()
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

-- | Run a given example using a given Selenium session.
withBrowser :: forall m context a. WebDriverMonad m context => Browser -> ExampleT (ContextWithSession context) m a -> ExampleT context m a
withBrowser browser (ExampleT readerMonad) = do
  WdSession {..} <- getContext webdriver
  -- Create new session if necessary (this can throw an exception)
  sess <- modifyMVar wdSessionMap $ \sessionMap -> case M.lookup browser sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      debug [i|Creating session for browser '#{browser}'|]
      sess'' <- liftIO $ W.mkSession wdConfig
      let sess' = sess'' { W.wdSessHistUpdate = W.unlimitedHistory }
      sess <- liftIO $ W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
      return (M.insert browser sess sessionMap, sess)

  ref <- liftIO $ newIORef sess

  -- Not used for now, but previous libraries have use a finally to grab the final session on exception.
  -- We could do the same here, but it's not clear that it's needed.
  let f :: m a -> m a = id

  ExampleT (withReaderT (\ctx -> LabelValue ref :> ctx) $ mapReaderT (mapLoggingT f) readerMonad)

-- | Convenience function. `withBrowser1 = withBrowser "browser1"`
withBrowser1 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withBrowser1 = withBrowser "browser1"

-- | Convenience function. `withBrowser2 = withBrowser "browser2"`
withBrowser2 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withBrowser2 = withBrowser "browser2"

getBrowsers :: (HasCallStack, HasLabel context "webdriver" WdSession, MonadIO m, MonadReader context m) => m [Browser]
getBrowsers = do
  WdSession {..} <- getContext webdriver
  M.keys <$> liftIO (readMVar wdSessionMap)
