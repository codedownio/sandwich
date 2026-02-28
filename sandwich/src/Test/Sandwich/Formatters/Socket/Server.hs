module Test.Sandwich.Formatters.Socket.Server (
  socketServer
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.String.Interpolate
import Data.Time
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import Test.Sandwich.Formatters.Socket.Commands
import Test.Sandwich.Types.RunTree
import UnliftIO.Exception


-- | Bidirectional Unix socket server. Each client connection reads line-based
-- commands and sends back responses terminated by a line containing just ".".
socketServer :: FilePath -> [RunNode BaseContext] -> TChan (Int, String, LogEntry) -> IO ()
socketServer socketPath rts logBroadcast = do
  -- Clean up any existing socket file
  removeFile socketPath `catch` \(_ :: IOError) -> return ()

  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix socketPath)
    listen sock 5
    forever $ do
      (conn, _) <- accept sock
      void $ async $ handleConnection conn rts logBroadcast

handleConnection :: Socket -> [RunNode BaseContext] -> TChan (Int, String, LogEntry) -> IO ()
handleConnection conn rts logBroadcast = do
  bufRef <- newIORef BS.empty
  handle (\(_ :: IOError) -> close conn) $ do
    sendAll conn "Connected to sandwich socket formatter. Type \"help\" for commands.\n\n> "
    forever $ do
      line <- readLine conn bufRef
      now <- getCurrentTime
      let cmd = BS8.unpack (stripCR line)
      case words cmd of
        ["stream-logs"] -> streamLogs conn logBroadcast
        _ -> do
          response <- handleCommand rts now cmd
          unless (null response) $ do
            sendAll conn (BS8.pack response)
            sendAll conn "\n"
          sendAll conn "> "

-- | Stream all log entries to the client until the connection is closed.
-- This is a blocking operation that never returns to the command loop.
streamLogs :: Socket -> TChan (Int, String, LogEntry) -> IO ()
streamLogs conn broadcastChan = do
  sendAll conn "Streaming logs (disconnect to stop)...\n"
  -- Duplicate the broadcast channel to get our own read position
  chan <- atomically $ dupTChan broadcastChan
  handle (\(_ :: IOError) -> return ()) $ forever $ do
    (nodeId, nodeLabel, LogEntry {..}) <- atomically $ readTChan chan
    let levelStr :: String
        levelStr = case logEntryLevel of
          LevelDebug -> "DEBUG"
          LevelInfo -> "INFO"
          LevelWarn -> "WARN"
          LevelError -> "ERROR"
          LevelOther t -> show t
        msgStr = BS8.unpack (fromLogStr logEntryStr)
        formatted = [i|#{show logEntryTime} [#{levelStr}] [#{nodeId}] #{nodeLabel}: #{msgStr}\n|]
    sendAll conn (BS8.pack formatted)

-- | Read a single line from the socket, buffering leftover bytes.
-- Returns the line without the trailing newline.
readLine :: Socket -> IORef BS.ByteString -> IO BS.ByteString
readLine conn bufRef = do
  buf <- readIORef bufRef
  case BS8.elemIndex '\n' buf of
    Just idx -> do
      let (line, rest) = BS.splitAt idx buf
      writeIORef bufRef (BS.drop 1 rest) -- skip the '\n'
      return line
    Nothing -> do
      chunk <- recv conn 4096
      when (BS.null chunk) $ throwIO (userError "connection closed")
      writeIORef bufRef (buf <> chunk)
      readLine conn bufRef

-- | Strip trailing carriage return (for clients that send \r\n)
stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
  | BS.null bs = bs
  | BS8.last bs == '\r' = BS.init bs
  | otherwise = bs
