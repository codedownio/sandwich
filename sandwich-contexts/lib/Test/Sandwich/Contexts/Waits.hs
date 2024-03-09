{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.Contexts.Waits where

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


timePerRequest :: Int
timePerRequest = 10000000

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
      }

-- | Send HTTP requests to url until we get a response with an given code
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

waitUntil200 :: (WaitConstraints m) => String -> m ()
waitUntil200 = waitUntilStatusCode (2, 0, 0) YesVerify

-- | Same as waitUntil200, but with a fixed timeout
waitUntil200WithTimeout :: (WaitConstraints m) => String -> m ()
waitUntil200WithTimeout = waitUntil200WithTimeout' 30_000_000

-- | Same as waitUntil200WithTimeout, but with a customizable timeout
waitUntil200WithTimeout' :: (WaitConstraints m) => Int -> String -> m ()
waitUntil200WithTimeout' timeInMicroseconds url = do
  maybeSuccess <- timeout timeInMicroseconds $ waitUntil200 url
  when (isNothing maybeSuccess) $
    expectationFailure [i|Failed to connect to URL "#{url}" in waitUntil200WithTimeout'...|]

waitUntilStatusCodeWithTimeout :: (WaitConstraints m) => (Int, Int, Int) -> String -> m ()
waitUntilStatusCodeWithTimeout code = waitUntilStatusCodeWithTimeout' 30000000 code YesVerify

-- | Same as waitUntilStatusCodeWithTimeout, but with a customizable timeout
waitUntilStatusCodeWithTimeout' :: (WaitConstraints m) => Int -> (Int, Int, Int) -> VerifyCerts -> String -> m ()
waitUntilStatusCodeWithTimeout' timeInMicroseconds code verifyCerts url = do
  maybeSuccess <- timeout timeInMicroseconds $ waitUntilStatusCode code verifyCerts url
  when (isNothing maybeSuccess) $
    expectationFailure [i|Failed to connect to URL "#{url}" in waitUntilStatusCodeWithTimeout'...|]


-- | Keep trying an action up to a timeout while it
-- a) fails with a FailureReason in the MonadError monad
waitUntil :: forall m a. (HasCallStack, MonadUnliftIO m) => Double -> m a -> m a
waitUntil timeInSeconds action = do
  startTime <- liftIO getCurrentTime

  recoveringDynamic policy [handleFailureReasonException startTime] $ \_status ->
    rethrowTimeoutExceptionWithCallStack $
      timeout (round (timeInSeconds * 1_000_000)) action >>= \case
        Nothing -> expectationFailure [i|Action timed out in waitUntil|]
        Just x -> return x

  where
    policy = capDelay 1_000_000 $ exponentialBackoff 1_000

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

#if !MIN_VERSION_time(1,9,1)
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac
#endif
