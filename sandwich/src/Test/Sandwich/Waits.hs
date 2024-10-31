{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

{-|

This module contains helper functions for waiting.

It can be very useful in tests to retry something, with a reasonable backoff policy to prevent the test from consuming lots of CPU while waiting.

-}


module Test.Sandwich.Waits (
  -- * General waits
  waitUntil
  , waitUntil'
  , defaultRetryPolicy
  ) where

import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Time
import Data.Typeable
import GHC.Stack
import System.Timeout (Timeout)
import Test.Sandwich
import UnliftIO.Exception
import UnliftIO.Retry
import UnliftIO.Timeout


-- | Keep trying an action up to a timeout while it fails with a 'FailureReason'.
-- Use exponential backoff, with delays capped at 1 second.
waitUntil :: forall m a. (HasCallStack, MonadUnliftIO m) => Double -> m a -> m a
waitUntil = waitUntil' defaultRetryPolicy

-- | The default retry policy.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = capDelay 1_000_000 $ exponentialBackoff 1_000

-- | Same as 'waitUntil', but with a configurable retry policy.
waitUntil' :: forall m a. (HasCallStack, MonadUnliftIO m) => RetryPolicy -> Double -> m a -> m a
waitUntil' policy timeInSeconds action = do
  startTime <- liftIO getCurrentTime

  recoveringDynamic policy [handleFailureReasonException startTime] $ \_status ->
    rethrowTimeoutExceptionWithCallStack $
      timeout (round (timeInSeconds * 1_000_000)) action >>= \case
        Nothing -> expectationFailure [i|Action timed out in waitUntil|]
        Just x -> return x

  where
    handleFailureReasonException startTime _status = Handler $ \(_ :: FailureReason) ->
      retryUnlessTimedOut startTime

    retryUnlessTimedOut :: UTCTime -> m RetryAction
    retryUnlessTimedOut startTime = do
      now <- liftIO getCurrentTime
      let thresh = secondsToNominalDiffTime (realToFrac timeInSeconds)
      if | (diffUTCTime now startTime) > thresh -> return DontRetry
         | otherwise -> return ConsultPolicy

    rethrowTimeoutExceptionWithCallStack :: (HasCallStack) => m a -> m a
    rethrowTimeoutExceptionWithCallStack = handleSyncOrAsync $ \(e@(SomeException inner)) ->
      if | Just (_ :: Timeout) <- fromExceptionUnwrap e -> do
             throwIO $ Reason (Just (popCallStack callStack)) "Timeout in waitUntil"
         | Just (SyncExceptionWrapper (cast -> Just (SomeException (cast -> Just (SomeAsyncException (cast -> Just (_ :: Timeout))))))) <- cast inner -> do
             throwIO $ Reason (Just (popCallStack callStack)) "Timeout in waitUntil"
         | otherwise -> do
             throwIO e
