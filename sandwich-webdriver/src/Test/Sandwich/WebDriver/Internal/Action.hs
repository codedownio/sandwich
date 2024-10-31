{-# LANGUAGE DataKinds #-}

module Test.Sandwich.WebDriver.Internal.Action where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
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
closeSession :: (HasCallStack, MonadLogger m, MonadUnliftIO m) => Session -> WebDriver -> m ()
closeSession session (WebDriver {wdSessionMap}) = do
  toClose <- modifyMVar wdSessionMap $ \sessionMap ->
    case M.lookup session sessionMap of
      Nothing -> return (sessionMap, Nothing)
      Just x -> return (M.delete session sessionMap, Just x)

  whenJust toClose $ \sess -> liftIO $ W.runWD sess W.closeSession

-- | Close all sessions except those listed.
closeAllSessionsExcept :: (HasCallStack, MonadLogger m, MonadUnliftIO m) => [Session] -> WebDriver -> m ()
closeAllSessionsExcept toKeep (WebDriver {wdSessionMap}) = do
  toClose <- modifyMVar wdSessionMap $ return . M.partitionWithKey (\name _ -> name `elem` toKeep)

  forM_ (M.toList toClose) $ \(name, sess) ->
    catch (liftIO $ W.runWD sess W.closeSession)
          (\(e :: SomeException) -> warn [i|Failed to destroy session '#{name}': '#{e}'|])

-- | Close all sessions.
closeAllSessions :: (HasCallStack, MonadLogger m, MonadUnliftIO m) => WebDriver -> m ()
closeAllSessions = closeAllSessionsExcept []

-- | Close the current session.
closeCurrentSession :: (
  MonadLogger m, WebDriverSessionMonad m context
  ) => m ()
closeCurrentSession = do
  webDriver <- getContext webdriver
  (session, _) <- getContext webdriverSession
  closeSession session webDriver
