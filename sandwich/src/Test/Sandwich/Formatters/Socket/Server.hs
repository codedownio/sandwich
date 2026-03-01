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
import Data.Word
import GHC.Stats
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import Test.Sandwich.Formatters.Socket.Commands
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception


-- | Bidirectional Unix socket server. Each client connection reads line-based
-- commands and sends back responses terminated by a line containing just ".".
socketServer :: FilePath -> [RunNode BaseContext] -> TChan (Int, String, LogEntry) -> TChan NodeEvent -> IO ()
socketServer socketPath rts logBroadcast eventBroadcast = do
  -- Clean up any existing socket file
  removeFile socketPath `catch` \(_ :: IOError) -> return ()

  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix socketPath)
    listen sock 5
    forever $ do
      (conn, _) <- accept sock
      void $ async $ handleConnection conn rts logBroadcast eventBroadcast

handleConnection :: Socket -> [RunNode BaseContext] -> TChan (Int, String, LogEntry) -> TChan NodeEvent -> IO ()
handleConnection conn rts logBroadcast eventBroadcast = do
  bufRef <- newIORef BS.empty
  handle (\(_ :: IOError) -> close conn) $ do
    sendAll conn "Connected to sandwich socket formatter. Type \"help\" for commands.\n\n> "
    forever $ do
      line <- readLine conn bufRef
      now <- getCurrentTime
      let cmd = BS8.unpack (stripCR line)
      case words cmd of
        ["stream-logs"] -> streamLogs conn logBroadcast
        ["stream-events"] -> streamEvents conn eventBroadcast
        ["stream-rts-stats"] -> streamRtsStats conn
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
        msgStr = BS8.unpack logEntryStr
        formatted = [i|#{show logEntryTime} [#{levelStr}] [#{nodeId}] #{nodeLabel}: #{msgStr}\n|]
    sendAll conn (BS8.pack formatted)

-- | Stream node lifecycle events (started, done) to the client.
streamEvents :: Socket -> TChan NodeEvent -> IO ()
streamEvents conn broadcastChan = do
  sendAll conn "Streaming events (disconnect to stop)...\n"
  chan <- atomically $ dupTChan broadcastChan
  handle (\(_ :: IOError) -> return ()) $ forever $ do
    NodeEvent {..} <- atomically $ readTChan chan
    let typeStr :: String
        typeStr = case nodeEventType of
          EventStarted -> "STARTED"
          EventDone Success -> "DONE:OK"
          EventDone (Failure (Pending {})) -> "DONE:PENDING"
          EventDone (Failure reason) -> [i|DONE:FAIL: #{showFailureReasonBrief reason}|]
          EventDone DryRun -> "DONE:DRYRUN"
          EventDone Cancelled -> "DONE:CANCELLED"
        formatted = [i|#{show nodeEventTime} [#{nodeEventId}] #{nodeEventLabel}: #{typeStr}\n|]
    sendAll conn (BS8.pack formatted)

showFailureReasonBrief :: FailureReason -> String
showFailureReasonBrief (Reason {failureReason}) = failureReason
showFailureReasonBrief (ChildrenFailed {failureNumChildren}) = [i|#{failureNumChildren} children failed|]
showFailureReasonBrief _ = "(see node detail)"

-- | Stream RTS stats to the client every second until the connection is closed.
-- Requires the program to be run with +RTS -T for stats to be available.
streamRtsStats :: Socket -> IO ()
streamRtsStats conn = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then sendAll conn "RTS stats not available. Run with +RTS -T to enable.\n> "
    else do
      sendAll conn "Streaming RTS stats every 1s (disconnect to stop)...\n"
      handle (\(_ :: IOError) -> return ()) $ forever $ do
        stats <- getRTSStats
        let gc' = gc stats
            line = formatRtsStats stats gc'
        sendAll conn (BS8.pack line)
        threadDelay 1000000

formatRtsStats :: RTSStats -> GCDetails -> String
formatRtsStats stats gc' = unlines
  [ [i|live_bytes:         #{formatBytes (gcdetails_live_bytes gc')}|]
  , [i|heap_size:          #{formatBytes (gcdetails_mem_in_use_bytes gc')}|]
  , [i|allocated_bytes:    #{formatBytes (allocated_bytes stats)}|]
  , [i|max_live_bytes:     #{formatBytes (max_live_bytes stats)}|]
  , [i|large_objects:      #{formatBytes (gcdetails_large_objects_bytes gc')}|]
  , [i|compact_bytes:      #{formatBytes (gcdetails_compact_bytes gc')}|]
  , [i|slop_bytes:         #{formatBytes (gcdetails_slop_bytes gc')}|]
  , [i|gcs:                #{gcs stats}|]
  , [i|major_gcs:          #{major_gcs stats}|]
  , [i|gc_cpu:             #{nsToMs (gc_cpu_ns stats)}ms|]
  , [i|mutator_cpu:        #{nsToMs (mutator_cpu_ns stats)}ms|]
  , ""
  ]
  where
    nsToMs :: RtsTime -> RtsTime
    nsToMs ns = ns `div` 1000000

formatBytes :: Word64 -> String
formatBytes b
  | b < 1024 = [i|#{b} B|]
  | b < 1024 * 1024 = [i|#{b `div` 1024} KiB (#{b})|]
  | b < 1024 * 1024 * 1024 = [i|#{b `div` (1024 * 1024)} MiB (#{b})|]
  | otherwise = [i|#{b `div` (1024 * 1024 * 1024)} GiB (#{b})|]

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
