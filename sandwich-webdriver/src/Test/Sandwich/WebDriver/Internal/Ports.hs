{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, LambdaCase #-}

module Test.Sandwich.WebDriver.Internal.Ports (
  findFreePortOrException
  ) where

import Control.Exception
import Control.Retry
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Network.Socket
import System.Random (randomRIO)
import Test.Sandwich.WebDriver.Internal.Util

firstUserPort :: PortNumber
firstUserPort = 1024

highestPort :: PortNumber
highestPort = 65535

-- |Find an unused port in a given range
findFreePortInRange' :: RetryPolicy -> IO PortNumber -> IO (Maybe PortNumber)
findFreePortInRange' policy getAcceptableCandidate = retrying policy (\_retryStatus result -> return $ isNothing result) (const findFreePortInRange'')
  where
    findFreePortInRange'' :: IO (Maybe PortNumber)
    findFreePortInRange'' = do
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

findFreePortInRange :: IO PortNumber -> IO (Maybe PortNumber)
findFreePortInRange = findFreePortInRange' (limitRetries 50)

-- | Find an unused port in the ephemeral port range.
-- See https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
-- This works without a timeout since there should always be a port in the somewhere;
-- it might be advisable to wrap in a timeout anyway.
findFreePort :: IO (Maybe PortNumber)
findFreePort = findFreePortInRange getNonEphemeralCandidate

findFreePortOrException :: IO PortNumber
findFreePortOrException = findFreePort >>= \case
  Just port -> return port
  Nothing -> error "Couldn't find free port"

-- * Util

getNonEphemeralCandidate :: IO PortNumber
getNonEphemeralCandidate = do
  (ephemeralStart, ephemeralEnd) <- getEphemeralPortRange >>= \case
    Left _ -> return (49152, 65535)
    Right range -> return range

  let numBelow = ephemeralStart - firstUserPort
  let numAbove = highestPort - ephemeralEnd

  u :: Double <- randomRIO (0, 1)

  let useLowerRange = u < ((fromIntegral numBelow) / (fromIntegral numBelow + fromIntegral numAbove))

  if | useLowerRange -> fromInteger <$> randomRIO (fromIntegral firstUserPort, fromIntegral ephemeralStart)
     | otherwise -> fromInteger <$> randomRIO (fromIntegral ephemeralEnd, fromIntegral highestPort)

getEphemeralPortRange :: IO (Either T.Text (PortNumber, PortNumber))
getEphemeralPortRange = leftOnException' $ do
  contents <- readFile "/proc/sys/net/ipv4/ip_local_port_range"
  case fmap read (words contents) of
    [p1, p2] -> return (p1, p2)
    _ -> error [i|Unexpected contents: '#{contents}'|]
