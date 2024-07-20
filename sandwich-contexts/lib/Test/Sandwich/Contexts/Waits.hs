{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.Contexts.Waits (
  -- * General waits
  waitUntil
  , waitUntil'
  , defaultRetryPolicy

  -- * HTTP waits
  , waitUntilStatusCode
  , waitUntilStatusCodeWithTimeout

  -- * Types
  , VerifyCerts(..)
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Maybe
import Data.String.Interpolate
import Data.Time
import Data.Typeable
import GHC.Stack
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Network.Stream hiding (Result)
import Relude
import System.Timeout (Timeout)
import Test.Sandwich
import UnliftIO.Exception
import UnliftIO.Retry
import UnliftIO.Timeout

#if MIN_VERSION_crypton_connection(0,4,0)
import Data.Default (def)
#endif


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


timePerRequest :: Int
timePerRequest = 10_000_000

type WaitConstraints m = (HasCallStack, MonadLogger m, MonadUnliftIO m, MonadThrow m)

data VerifyCerts = YesVerify | NoVerify
  deriving (Eq)

tlsNoVerifySettings :: ManagerSettings
tlsNoVerifySettings = mkManagerSettings tlsSettings Nothing
  where
    tlsSettings = TLSSettingsSimple {
      settingDisableCertificateValidation = True
      , settingDisableSession = False
      , settingUseServerName = False
#if MIN_VERSION_crypton_connection(0,4,0)
      , settingsClientSupported = def
#endif
      }

-- | Send HTTP requests to url until we get a response with an given code.
waitUntilStatusCode :: (WaitConstraints m) => (Int, Int, Int) -> VerifyCerts -> String -> m ()
waitUntilStatusCode code verifyCerts url = do
  debug [i|Beginning waitUntilStatusCode request to #{url}|]
  req <- parseRequest url
  man <- liftIO $ newManager (if verifyCerts == YesVerify then tlsManagerSettings else tlsNoVerifySettings)
  timeout timePerRequest (handleException $ (Right <$>) $ httpLbs req man) >>= \case
    Just (Right resp)
      | statusCode (responseStatus resp) == statusToInt code -> return ()
      | otherwise -> do
          debug [i|Unexpected response in waitUntilStatusCode request to #{url}: #{responseStatus resp}. Wanted #{code}. Body is #{responseBody resp}|]
          retry
    Just (Left err) -> do
      debug [i|Failure in waitUntilStatusCode request to #{url}: #{err}|]
      retry
    Nothing -> do
      debug [i|Timeout in waitUntilStatusCode request to #{url} (after #{timePerRequest}us)|]
      retry
  where
    retry = liftIO (threadDelay 1_000_000) >> waitUntilStatusCode code verifyCerts url
    handleException = handle (\(e :: SomeException) -> return $ Left $ ErrorMisc [i|Exception in waitUntilStatusCode: #{e}|])
    statusToInt (x, y, z) = 100 * x + 10 * y + z

-- | Same as 'waitUntilStatusCode', but with a customizable timeout in microseconds.
waitUntilStatusCodeWithTimeout :: (WaitConstraints m) => (Int, Int, Int) -> Int -> VerifyCerts -> String -> m ()
waitUntilStatusCodeWithTimeout code timeInMicroseconds verifyCerts url = do
  maybeSuccess <- timeout timeInMicroseconds $ waitUntilStatusCode code verifyCerts url
  when (isNothing maybeSuccess) $
    expectationFailure [i|Failed to connect to URL "#{url}" in waitUntilStatusCodeWithTimeout'...|]


#if !MIN_VERSION_time(1,9,1)
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac
#endif
