{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

{-|

HTTP(S)-specific wait functions, for waiting on servers.

-}


module Test.Sandwich.Contexts.HttpWaits (
  -- * HTTP waits
  waitUntilStatusCode
  , waitUntilStatusCodeWithTimeout

  -- * Types
  , VerifyCerts(..)
  , WaitConstraints
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Maybe
import Data.String.Interpolate
import GHC.Stack
import Network.Connection (TLSSettings(..))
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Network.Stream hiding (Result)
import Relude
import Test.Sandwich
import UnliftIO.Exception
import UnliftIO.Timeout

#ifdef MIN_VERSION_crypton_connection
#if MIN_VERSION_crypton_connection(0,4,0)
import Data.Default (def)
#endif
#endif


timePerRequest :: Int
timePerRequest = 10_000_000

type WaitConstraints m = (HasCallStack, MonadLogger m, MonadUnliftIO m, MonadThrow m)

-- | Whether to verify certificates or not when connecting to an HTTPS endpoint.
data VerifyCerts = YesVerify | NoVerify
  deriving (Eq)

tlsNoVerifySettings :: ManagerSettings
tlsNoVerifySettings = mkManagerSettings tlsSettings Nothing
  where
    tlsSettings = TLSSettingsSimple {
      settingDisableCertificateValidation = True
      , settingDisableSession = False
      , settingUseServerName = False
#ifdef MIN_VERSION_crypton_connection
#if MIN_VERSION_crypton_connection(0,4,0)
      , settingClientSupported = def
#endif
#endif
      }

-- | Send HTTP requests to a URL until we get a response with an given code.
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
