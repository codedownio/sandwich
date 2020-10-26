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
  introduceWebDriver

  -- * Running an example in a given session
  , withSession
  , withSession1
  , withSession2

  -- * Managing sessions
  , getSessions
  , closeSession
  , closeAllSessions
  , closeAllSessionsExcept
  , Session

  -- * Lower-level allocation functions
  , allocateWebDriver
  , allocateWebDriver'
  , cleanupWebDriver
  , cleanupWebDriver'

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Class
  , module Test.Sandwich.WebDriver.Config
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
import Test.Sandwich
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.Binaries
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Class
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W


-- | This is the main 'introduce' method for creating a WebDriver.
introduceWebDriver :: (HasBaseContext context, MonadIO m, MonadCatch m, MonadBaseControl IO m, MonadMask m) => WdOptions -> SpecFree (LabelValue "webdriver" WebDriver :> context) m () -> SpecFree context m ()
introduceWebDriver wdOptions = introduce "Introduce WebDriver session" webdriver (allocateWebDriver wdOptions) cleanupWebDriver

-- | Allocate a WebDriver using the given options
allocateWebDriver :: (HasBaseContext context, BaseMonad m) => WdOptions -> ExampleT context m WebDriver
allocateWebDriver wdOptions = do
  debug "Beginning allocateWebDriver"
  maybeRunRoot <- getRunRoot
  let runRoot = fromMaybe "/tmp" maybeRunRoot
  startWebDriver wdOptions runRoot

-- | Allocate a WebDriver using the given options and putting logs under the given path
allocateWebDriver' :: FilePath -> WdOptions -> IO WebDriver
allocateWebDriver' runRoot wdOptions = do
  runNoLoggingT $ startWebDriver wdOptions runRoot

-- | Clean up the given WebDriver
cleanupWebDriver :: (HasBaseContext context, BaseMonad m) => WebDriver -> ExampleT context m ()
cleanupWebDriver sess = do
  closeAllSessions sess
  stopWebDriver sess

-- | Clean up the given WebDriver without logging
cleanupWebDriver' :: WebDriver -> IO ()
cleanupWebDriver' sess = do
  runNoLoggingT $ do
    closeAllSessions sess
    stopWebDriver sess

-- | Run a given example using a given Selenium session.
withSession :: forall m context a. WebDriverMonad m context => Session -> ExampleT (ContextWithSession context) m a -> ExampleT context m a
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

  ExampleT (withReaderT (\ctx -> LabelValue ref :> ctx) $ mapReaderT (mapLoggingT f) readerMonad)

-- | Convenience function. 'withSession1' = 'withSession' "session1"
withSession1 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession1 = withSession "session1"

-- | Convenience function. 'withSession2' = 'withSession' "session2"
withSession2 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession2 = withSession "session2"

-- | Get all existing session names
getSessions :: (WebDriverSessionMonad m context) => m [Session]
getSessions = do
  WebDriver {..} <- getContext webdriver
  M.keys <$> liftIO (readMVar wdSessionMap)
