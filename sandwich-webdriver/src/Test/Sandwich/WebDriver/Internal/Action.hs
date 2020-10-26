{-# LANGUAGE ViewPatterns, LambdaCase, QuasiQuotes, RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
-- |

module Test.Sandwich.WebDriver.Internal.Action where

import Control.Concurrent.MVar.Lifted
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Sandwich.Logging
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W

-- | Close the given sessions
closeSession :: (HasCallStack, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => Session -> WebDriver -> m ()
closeSession session (WebDriver {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    whenJust (M.lookup session sessionMap) $ \sess ->
      liftIO $ W.runWD sess W.closeSession
    return $ M.delete session sessionMap

-- | Close all sessions except those listed
closeAllSessionsExcept :: (HasCallStack, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => [Session] -> WebDriver -> m ()
closeAllSessionsExcept toKeep (WebDriver {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    forM_ (M.toList sessionMap) $ \(name, sess) -> unless (name `elem` toKeep) $
      catch (liftIO $ W.runWD sess W.closeSession)
            (\(e :: SomeException) -> warn [i|Failed to destroy session '#{name}': '#{e}'|])
    return $ M.fromList [(b, s) | (b, s) <- M.toList sessionMap, b `elem` toKeep]

-- | Close all sessions
closeAllSessions :: (HasCallStack, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => WebDriver -> m ()
closeAllSessions = closeAllSessionsExcept []
