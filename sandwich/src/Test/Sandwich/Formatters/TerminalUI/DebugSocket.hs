module Test.Sandwich.Formatters.TerminalUI.DebugSocket (
  debugSocketServer
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import System.Directory (removeFile)
import UnliftIO.Exception


-- | Debug socket server that accepts connections and broadcasts events.
-- Connect with @nc -U \<path\>@ to receive line-oriented debug events.
debugSocketServer :: FilePath -> TChan ByteString -> IO ()
debugSocketServer socketPath chan = do
  -- Clean up any existing socket file
  removeFile socketPath `catch` \(_ :: IOError) -> return ()

  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix socketPath)
    listen sock 5
    forever $ do
      (conn, _) <- accept sock
      -- Spawn a thread to handle this connection
      void $ async $ handleConnection conn
  where
    handleConnection conn = do
      -- Duplicate the channel so this client gets its own read position
      chan' <- atomically $ dupTChan chan
      -- Send events until error (client disconnect)
      handle (\(_ :: IOError) -> close conn) $ forever $ do
        msg <- atomically $ readTChan chan'
        sendAll conn msg
