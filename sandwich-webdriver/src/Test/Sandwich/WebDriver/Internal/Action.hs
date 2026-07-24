{-# LANGUAGE DataKinds #-}

module Test.Sandwich.WebDriver.Internal.Action where

import Control.Monad
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Timeout


-- | How long to wait for a single session's WebDriver-side close before giving up and leaving it in
-- the session map (still tracked, still consistent with W's _webDriverSessions).
sessionCloseTimeoutUs :: Int
sessionCloseTimeoutUs = 30 * 1000000 -- 30s

-- | Close the given session.
closeSession :: (MonadLogger m, W.WebDriverBase m) => SessionName -> TestWebDriverContext -> m ()
closeSession session (TestWebDriverContext {wdSessionMap, wdContext}) =
  modifyMVar_ wdSessionMap $ \sessionMap ->
    case M.lookup session sessionMap of
      Nothing -> return sessionMap
      Just (SessionMapEntry {..}) ->
        tryAny (timeout sessionCloseTimeoutUs (W.closeSession wdContext sessionMapEntrySession)) >>= \case
          Right (Just ()) -> do
            info [i|Closed session: #{sessionMapEntrySession}|]
            forM_ sessionMapEntryDirsToRemove $ \dirToRemove -> do
              debug [i|Removing session-specific directory: #{dirToRemove}|]
              catch (removePathForcibly dirToRemove)
                    (\(e :: SomeException) -> warn [i|Failed to remove session directory '#{dirToRemove}': '#{e}'|])
            return (M.delete session sessionMap)
          Right Nothing -> do
            warn [i|Timed out after #{sessionCloseTimeoutUs `div` 1000000}s closing session '#{session}'. Leaving session in map.|]
            return sessionMap
          Left e -> do
            warn [i|Failed to destroy session '#{session}': '#{e}'. Leaving session in map.|]
            return sessionMap

-- | Close all sessions except those listed.
closeAllSessionsExcept :: (HasCallStack, MonadLogger m, W.WebDriverBase m) => [SessionName] -> TestWebDriverContext -> m ()
closeAllSessionsExcept toKeep (TestWebDriverContext {wdSessionMap, wdContext}) =
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    let (toKeepMap, toCloseMap) = M.partitionWithKey (\name _ -> name `elem` toKeep) sessionMap

    stillOpen <- fmap (M.fromList . catMaybes) $ forM (M.toList toCloseMap) $ \(name, entry@(SessionMapEntry {..})) ->
      tryAny (timeout sessionCloseTimeoutUs (W.closeSession wdContext sessionMapEntrySession)) >>= \case
        Right (Just ()) -> do
          forM_ sessionMapEntryDirsToRemove $ \dirToRemove -> do
            debug [i|Removing session-specific directory: #{dirToRemove}|]
            catch (removePathForcibly dirToRemove)
                  (\(e :: SomeException) -> warn [i|Failed to remove session directory '#{dirToRemove}': '#{e}'|])
          return Nothing
        Right Nothing -> do
          warn [i|Timed out after #{sessionCloseTimeoutUs `div` 1000000}s closing session '#{name}'|]
          return (Just (name, entry))
        Left e -> do
          warn [i|Failed to destroy session '#{name}': '#{e}'|]
          return (Just (name, entry))

    return (toKeepMap <> stillOpen)

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
