{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Test.Sandwich.Contexts.Kubernetes.Util.Ports where

import Control.Monad.Catch (MonadCatch, catch)
import Control.Retry
import Network.Socket
import Relude
import System.Random (randomRIO)


-- | Find an unused port in a given range
findFreePortInRange' :: forall m. (
  MonadIO m, MonadCatch m
  ) => RetryPolicy -> (PortNumber, PortNumber) -> [PortNumber] -> m (Maybe PortNumber)
findFreePortInRange' retryPolicy (start, end) blacklist = retrying retryPolicy (\_retryStatus result -> return $ isNothing result) (const findFreePortInRange')
  where getAcceptableCandidate :: m PortNumber
        getAcceptableCandidate = do
          candidate <- liftIO (fromInteger <$> randomRIO (fromIntegral start, fromIntegral end))
          if | candidate `elem` blacklist -> getAcceptableCandidate
             | otherwise -> return candidate

        findFreePortInRange' :: m (Maybe PortNumber)
        findFreePortInRange' = do
          candidate <- getAcceptableCandidate
          isPortFree candidate >>= \case
            False -> return Nothing
            True -> return $ Just candidate

isPortFree :: (MonadIO m, MonadCatch m) => PortNumber -> m Bool
isPortFree candidate = catch (tryOpenAndClosePort candidate >> return True)
                             (\(_ :: SomeException) -> return False)

tryOpenAndClosePort :: MonadIO m => PortNumber -> m PortNumber
tryOpenAndClosePort port = liftIO $ do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
  getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port) >>= \case
    ((AddrInfo {addrAddress=addr}):_) -> do
      bind sock addr
      close sock
      return $ fromIntegral port
    [] -> error "Couldn't resolve address 127.0.0.1"

findFreePortInRange :: (
  MonadIO m, MonadCatch m
  ) => (PortNumber, PortNumber) -> [PortNumber] -> m (Maybe PortNumber)
findFreePortInRange = findFreePortInRange' (limitRetries 50)

-- | Find an unused port in the ephemeral port range.
-- See https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
-- This works without a timeout since there should always be a port in there somewhere;
-- it might be advisable to wrap in a timeout anyway.
findFreePort :: (MonadIO m, MonadCatch m) => m (Maybe PortNumber)
findFreePort = findFreePortInRange (49152, 65535) []

findFreePortOrException :: (MonadIO m, MonadCatch m) => m PortNumber
findFreePortOrException = findFreePortOrException' (const True)

findFreePortOrException' :: (MonadIO m, MonadCatch m) => (PortNumber -> Bool) -> m PortNumber
findFreePortOrException' isAcceptable = findFreePort >>= \case
  Just port
    | isAcceptable port -> return port
    | otherwise -> findFreePortOrException' isAcceptable
  Nothing -> error "Couldn't find free port"

findFreePortNotIn :: (MonadIO m, MonadCatch m) => [PortNumber] -> m (Maybe PortNumber)
findFreePortNotIn = findFreePortInRange (49152, 65535)
