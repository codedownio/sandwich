{-# LANGUAGE RankNTypes #-}

module Test.Sandwich.ManagedAsync (
  managedAsync
  , managedAsyncWithUnmask
  , managedWithAsync
  , managedWithAsync_
  , AsyncInfo(..)
  , AsyncEvent(..)
  , asyncEventBroadcast
  , getManagedAsyncInfos
  ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
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
