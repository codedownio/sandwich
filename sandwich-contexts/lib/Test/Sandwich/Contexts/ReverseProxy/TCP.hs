{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | This module is inspired by the http-reverse-proxy package:
-- https://hackage.haskell.org/package/http-reverse-proxy

module Test.Sandwich.Contexts.ReverseProxy.TCP where

import Control.Monad.IO.Unlift
import Data.Conduit
import qualified Data.Conduit.Network as DCN
import qualified Data.Conduit.Network.Unix as DCNU
import Data.Streaming.Network (setAfterBind)
import Data.String.Interpolate
import Network.Socket
import Relude
import Test.Sandwich (expectationFailure)
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.Timeout


withProxyToUnixSocket :: MonadUnliftIO m => FilePath -> (PortNumber -> m a) -> m a
withProxyToUnixSocket socketPath f = do
  portVar <- newEmptyMVar
  let ss = DCN.serverSettings 0 "*"
         & setAfterBind (\sock -> do
             getSocketName sock >>= \case
               SockAddrInet port _ -> putMVar portVar port
               SockAddrInet6 port _ _ _ -> putMVar portVar port
               x -> expectationFailure [i|withProxyToUnixSocket: expected to bind a TCP socket, but got other addr: #{x}|]
           )
  withAsync (liftIO $ DCN.runTCPServer ss app `onException` (tryPutMVar portVar (-1))) $ \_ ->
    timeout 60_000_000 (readMVar portVar) >>= \case
      Nothing -> expectationFailure [i|withProxyToUnixSocket: didn't get port within 60s|]
      Just (-1) -> expectationFailure [i|withProxyToUnixSocket: TCP server threw exception|]
      Just port -> f port

  where
    app appdata = DCNU.runUnixClient (DCNU.clientSettings socketPath) $ \appdataServer ->
      concurrently_
        (runConduit $ DCN.appSource appdata .| DCN.appSink appdataServer)
        (runConduit $ DCN.appSource appdataServer .| DCN.appSink appdata)
