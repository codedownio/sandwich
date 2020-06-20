{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables #-}

module Test.Sandwich.WebDriver.Internal.Ports (
  findFreePortOrException
  ) where

import Control.Exception
import Control.Retry
import Data.Maybe
import Network.Socket
import System.Random (randomRIO)

-- |Find an unused port in a given range
findFreePortInRange' :: RetryPolicy -> (PortNumber, PortNumber) -> [PortNumber] -> IO (Maybe PortNumber)
findFreePortInRange' retryPolicy (start, end) blacklist = retrying retryPolicy (\_retryStatus result -> return $ isNothing result) (const findFreePortInRange')
  where getAcceptableCandidate :: IO PortNumber
        getAcceptableCandidate = do
          candidate <- (fromInteger) <$> randomRIO (fromIntegral start, fromIntegral end)
          if | candidate `elem` blacklist -> getAcceptableCandidate
             | otherwise -> return candidate

        findFreePortInRange' :: IO (Maybe PortNumber)
        findFreePortInRange' = do
          candidate <- getAcceptableCandidate
          catch (tryOpenAndClosePort candidate >> return (Just candidate)) (\(_ :: SomeException) -> return Nothing)
          where
            tryOpenAndClosePort :: PortNumber -> IO PortNumber
            tryOpenAndClosePort port = do
              sock <- socket AF_INET Stream 0
              setSocketOption sock ReuseAddr 1
              let hostAddress = tupleToHostAddress (127, 0, 0, 1)
              bind sock (SockAddrInet port hostAddress)
              close sock
              return $ fromIntegral port


findFreePortInRange :: (PortNumber, PortNumber) -> [PortNumber] -> IO (Maybe PortNumber)
findFreePortInRange = findFreePortInRange' (limitRetries 50)

-- |Find an unused port in the ephemeral port range.
-- See https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
-- This works without a timeout since there should always be a port in the somewhere;
-- it might be advisable to wrap in a timeout anyway.
findFreePort :: IO (Maybe PortNumber)
findFreePort = findFreePortInRange (49152, 65535) []

findFreePortOrException :: IO PortNumber
findFreePortOrException = findFreePort >>= \case
  Just port -> return port
  Nothing -> error "Couldn't find free port"

-- findFreePortNotIn :: [PortNumber] -> IO (Maybe PortNumber)
-- findFreePortNotIn = findFreePortInRange (49152, 65535)
