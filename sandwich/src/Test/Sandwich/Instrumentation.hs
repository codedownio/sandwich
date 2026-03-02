module Test.Sandwich.Instrumentation (
  streamLogsToFile
  , streamEventsToFile
  , streamRtsStatsToFile
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS8
import Data.String.Interpolate
import Data.Word
import GHC.Stats
import System.IO (IOMode(..), hFlush, hPutStr, hSetBuffering, BufferMode(..), withFile)
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import UnliftIO.Concurrent (threadDelay)


-- | Stream all log entries from a broadcast channel to a file.
streamLogsToFile :: FilePath -> TChan (Int, String, LogEntry) -> IO ()
streamLogsToFile path broadcastChan = do
  chan <- atomically $ dupTChan broadcastChan
  withFile path AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    forever $ do
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
      hPutStr h formatted
      hFlush h

-- | Stream node lifecycle events from a broadcast channel to a file.
streamEventsToFile :: FilePath -> TChan NodeEvent -> IO ()
streamEventsToFile path broadcastChan = do
  chan <- atomically $ dupTChan broadcastChan
  withFile path AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    forever $ do
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
      hPutStr h formatted
      hFlush h

-- | Poll RTS stats every second and append to a file.
-- Requires the program to be run with +RTS -T for stats to be available.
streamRtsStatsToFile :: FilePath -> IO ()
streamRtsStatsToFile path = do
  enabled <- getRTSStatsEnabled
  when enabled $ do
    withFile path AppendMode $ \h -> do
      hSetBuffering h LineBuffering
      forever $ do
        stats <- getRTSStats
        let gc' = gc stats
        hPutStr h (formatRtsStats stats gc')
        hFlush h
        threadDelay 1000000

showFailureReasonBrief :: FailureReason -> String
showFailureReasonBrief (Reason {failureReason}) = failureReason
showFailureReasonBrief (ChildrenFailed {failureNumChildren}) = [i|#{failureNumChildren} children failed|]
showFailureReasonBrief _ = "(see node detail)"

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
