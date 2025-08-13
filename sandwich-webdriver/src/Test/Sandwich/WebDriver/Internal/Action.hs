{-# LANGUAGE DataKinds #-}

module Test.Sandwich.WebDriver.Internal.Action where

import Control.Monad
import Control.Monad.Logger
import qualified Data.Map as M
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import UnliftIO.Concurrent
import UnliftIO.Exception


-- | Close the given session.
closeSession :: (MonadLogger m, W.WebDriverBase m) => SessionName -> TestWebDriverContext -> m ()
closeSession session (TestWebDriverContext {wdSessionMap, wdContext}) = do
  toClose <- modifyMVar wdSessionMap $ \sessionMap ->
    case M.lookup session sessionMap of
      Nothing -> return (sessionMap, Nothing)
      Just x -> return (M.delete session sessionMap, Just x)

  whenJust toClose $ \sess -> W.closeSession wdContext sess

-- | Close all sessions except those listed.
closeAllSessionsExcept :: (HasCallStack, MonadLogger m, W.WebDriverBase m) => [SessionName] -> TestWebDriverContext -> m ()
closeAllSessionsExcept toKeep (TestWebDriverContext {wdSessionMap, wdContext}) = do
  toClose <- modifyMVar wdSessionMap $ return . M.partitionWithKey (\name _ -> name `elem` toKeep)

  forM_ (M.toList toClose) $ \(name, sess) ->
    catch (W.closeSession wdContext sess)
          (\(e :: SomeException) -> warn [i|Failed to destroy session '#{name}': '#{e}'|])

-- | Close all sessions.
closeAllSessions :: (HasCallStack, MonadLogger m, W.WebDriverBase m) => TestWebDriverContext -> m ()
closeAllSessions = closeAllSessionsExcept []

-- | Close the current session.
closeCurrentSession :: (
  MonadLogger m, WebDriverSessionMonad m context, W.WebDriverBase m
  ) => m ()
closeCurrentSession = do
  webDriver <- getContext webdriver
  (session, _) <- getContext webdriverSession
  closeSession session webDriver
