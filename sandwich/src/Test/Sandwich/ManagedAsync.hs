{-# LANGUAGE RankNTypes #-}

module Test.Sandwich.ManagedAsync (
  -- * With explicit run ID
  managedAsync
  , managedAsyncWithUnmask
  , managedWithAsync
  , managedWithAsync_

  -- * With run ID from BaseContext
  , managedAsyncContext
  , managedAsyncWithUnmaskContext
  , managedWithAsyncContext
  , managedWithAsyncContext_

  -- * Types and utilities
  , AsyncInfo(..)
  , AsyncEvent(..)
  , asyncEventBroadcast
  , getManagedAsyncInfos
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Test.Sandwich.Types.RunTree (HasBaseContextMonad, BaseContext(..), getBaseContext)
import UnliftIO.Async
import UnliftIO.Exception


data AsyncInfo = AsyncInfo {
  asyncInfoThreadId :: !T.Text
  , asyncInfoParentThreadId :: !T.Text
  , asyncInfoName :: !T.Text
  , asyncInfoRunId :: !T.Text
  } deriving (Show, Eq)

data AsyncEvent = AsyncStarted !AsyncInfo | AsyncFinished !AsyncInfo

{-# NOINLINE allManagedAsyncs #-}
allManagedAsyncs :: IORef (M.Map ThreadId AsyncInfo)
allManagedAsyncs = unsafePerformIO $ newIORef M.empty

{-# NOINLINE asyncEventBroadcast #-}
asyncEventBroadcast :: TChan AsyncEvent
asyncEventBroadcast = unsafePerformIO newBroadcastTChanIO

getManagedAsyncInfos :: IO (M.Map T.Text AsyncInfo)
getManagedAsyncInfos = M.mapKeys (T.pack . show) <$> readIORef allManagedAsyncs

managedAsync :: MonadUnliftIO m => T.Text -> T.Text -> m a -> m (Async a)
managedAsync runId name action = do
  parentThreadId <- liftIO myThreadId
  async $ bracketedAction parentThreadId runId name action

managedAsyncWithUnmask :: MonadUnliftIO m => T.Text -> T.Text -> ((forall b. m b -> m b) -> m a) -> m (Async a)
managedAsyncWithUnmask runId name action = do
  parentThreadId <- liftIO myThreadId
  asyncWithUnmask $ \unmask -> bracketedAction parentThreadId runId name (action unmask)

managedWithAsync :: MonadUnliftIO m => T.Text -> T.Text -> m a -> (Async a -> m b) -> m b
managedWithAsync runId name action cb = do
  parentThreadId <- liftIO myThreadId
  withAsync (bracketedAction parentThreadId runId name action) cb

managedWithAsync_ :: MonadUnliftIO m => T.Text -> T.Text -> m a -> m b -> m b
managedWithAsync_ runId name f g = managedWithAsync runId name f (const g)

-- * With run ID from BaseContext

-- | Like 'managedAsync', but extracts the run ID from 'BaseContext'.
managedAsyncContext :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> m a -> m (Async a)
managedAsyncContext name action = do
  runId <- baseContextRunId <$> asks getBaseContext
  managedAsync runId name action

-- | Like 'managedAsyncWithUnmask', but extracts the run ID from 'BaseContext'.
managedAsyncWithUnmaskContext :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> ((forall b. m b -> m b) -> m a) -> m (Async a)
managedAsyncWithUnmaskContext name action = do
  runId <- baseContextRunId <$> asks getBaseContext
  managedAsyncWithUnmask runId name action

-- | Like 'managedWithAsync', but extracts the run ID from 'BaseContext'.
managedWithAsyncContext :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> m a -> (Async a -> m b) -> m b
managedWithAsyncContext name action cb = do
  runId <- baseContextRunId <$> asks getBaseContext
  managedWithAsync runId name action cb

-- | Like 'managedWithAsync_', but extracts the run ID from 'BaseContext'.
managedWithAsyncContext_ :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> m a -> m b -> m b
managedWithAsyncContext_ name f g = managedWithAsyncContext name f (const g)

-- * Internal

bracketedAction :: MonadUnliftIO m => ThreadId -> T.Text -> T.Text -> m a -> m a
bracketedAction parentThreadId runId name action = bracket record unrecord (const action)
  where
    record :: MonadUnliftIO m => m ThreadId
    record = do
      asyncThreadId <- liftIO myThreadId
      let info = AsyncInfo {
            asyncInfoThreadId = T.pack (show asyncThreadId)
            , asyncInfoParentThreadId = T.pack (show parentThreadId)
            , asyncInfoName = name
            , asyncInfoRunId = runId
            }
      liftIO $ atomicModifyIORef' allManagedAsyncs (\m -> (M.insert asyncThreadId info m, ()))
      liftIO $ atomically $ writeTChan asyncEventBroadcast (AsyncStarted info)
      return asyncThreadId

    unrecord :: MonadUnliftIO m => ThreadId -> m ()
    unrecord asyncThreadId = liftIO $ do
      mInfo <- atomicModifyIORef' allManagedAsyncs (\m -> (M.delete asyncThreadId m, M.lookup asyncThreadId m))
      case mInfo of
        Just info -> atomically $ writeTChan asyncEventBroadcast (AsyncFinished info)
        Nothing -> return ()
