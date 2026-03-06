module Test.Sandwich.Instrumentation (
  streamLogsToFile
  , streamEventsToFile
  , streamRtsStatsToFile
  , streamManagedAsyncEventsToFile
  , writeTreeFile

  , formatRtsStats
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time
import Data.Word
import Debug.Trace (traceMarkerIO)
import GHC.Stats
import System.IO (IOMode(..), hFlush, hPutStr, hSetBuffering, BufferMode(..), withFile)
import Test.Sandwich.ManagedAsync (AsyncEvent(..), AsyncInfo(..), getManagedAsyncInfos)
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception


-- | Stream all log entries from a broadcast channel to a file,
-- including the recursive heap size of each LogEntry.
-- When cancelled, writes a summary line with total bytes.
streamLogsToFile :: FilePath -> TChan (Int, String, LogEntry) -> IO ()
streamLogsToFile path broadcastChan = do
  chan <- atomically $ dupTChan broadcastChan
  totalRef <- newIORef (0 :: Word64)
  countRef <- newIORef (0 :: Int)
  withFile path AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    let loop = forever $ do
          (nodeId, nodeLabel, LogEntry {..}) <- atomically $ readTChan chan
          let entrySize = fromIntegral (BS8.length logEntryStr) :: Word64
          modifyIORef' totalRef (+ entrySize)
          modifyIORef' countRef (+ 1)
          let levelStr :: String
              levelStr = case logEntryLevel of
                LevelDebug -> "DEBUG"
                LevelInfo -> "INFO"
                LevelWarn -> "WARN"
                LevelError -> "ERROR"
                LevelOther t -> show t
              msgStr = BS8.unpack logEntryStr
              formatted = [i|#{show logEntryTime} [#{levelStr}] [#{nodeId}] #{nodeLabel}: #{msgStr} (#{entrySize} bytes)\n|]
          hPutStr h formatted
          hFlush h
    loop `finally` do
      total <- readIORef totalRef
      count <- readIORef countRef
      hPutStr h [i|\nTotal: #{count} log entries, #{formatBytes total} total log bytes\n|]
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
            EventSetupStarted -> "SETUP:STARTED"
            EventSetupFinished -> "SETUP:FINISHED"
            EventTeardownStarted -> "TEARDOWN:STARTED"
            EventTeardownFinished -> "TEARDOWN:FINISHED"
            EventMilestone msg -> [i|MILESTONE: #{msg}|]
          formatted = [i|#{show nodeEventTime} [#{nodeEventId}] #{nodeEventLabel}: #{typeStr}\n|]
      traceMarkerIO [i|[#{nodeEventId}] #{nodeEventLabel}: #{typeStr}|]
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
        now <- getCurrentTime
        stats <- getRTSStats
        let gc' = gc stats
        T.hPutStr h "\n\n"
        T.hPutStr h $ formatRtsStats now stats gc'
        hFlush h
        threadDelay 1000000

showFailureReasonBrief :: FailureReason -> String
showFailureReasonBrief (Reason {failureReason}) = failureReason
showFailureReasonBrief (ChildrenFailed {failureNumChildren}) = [i|#{failureNumChildren} children failed|]
showFailureReasonBrief _ = "(see node detail)"

formatRtsStats :: UTCTime -> RTSStats -> GCDetails -> Text
formatRtsStats now stats gc' = [__i|
  #{now}
  live_bytes:         #{formatBytes (gcdetails_live_bytes gc')}
  heap_size:          #{formatBytes (gcdetails_mem_in_use_bytes gc')}
  allocated_bytes:    #{formatBytes (allocated_bytes stats)}
  max_live_bytes:     #{formatBytes (max_live_bytes stats)}
  large_objects:      #{formatBytes (gcdetails_large_objects_bytes gc')}
  compact_bytes:      #{formatBytes (gcdetails_compact_bytes gc')}
  slop_bytes:         #{formatBytes (gcdetails_slop_bytes gc')}
  gcs:                #{gcs stats}
  major_gcs:          #{major_gcs stats}
  gc_cpu:             #{nsToMs (gc_cpu_ns stats)}ms
  mutator_cpu:        #{nsToMs (mutator_cpu_ns stats)}ms
  |]
  where
    nsToMs :: RtsTime -> RtsTime
    nsToMs ns = ns `div` 1000000

formatBytes :: Word64 -> String
formatBytes b
  | b < 1024 = [i|#{b} B|]
  | b < 1024 * 1024 = [i|#{b `div` 1024} KiB (#{b})|]
  | b < 1024 * 1024 * 1024 = [i|#{b `div` (1024 * 1024)} MiB (#{b})|]
  | otherwise = [i|#{b `div` (1024 * 1024 * 1024)} GiB (#{b})|]

-- | Stream managed async lifecycle events (started/finished) from a broadcast channel to a file.
-- When cancelled, writes a summary of all asyncs still alive at that point.
streamManagedAsyncEventsToFile :: FilePath -> TChan AsyncEvent -> IO ()
streamManagedAsyncEventsToFile path broadcastChan = do
  chan <- atomically $ dupTChan broadcastChan
  withFile path AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    let loop = forever $ do
          event <- atomically $ readTChan chan
          now <- getCurrentTime
          let line :: String
              line = case event of
                AsyncStarted info -> [i|#{show now} STARTED (#{asyncInfoThreadId info}, parent #{asyncInfoParentThreadId info}, #{asyncInfoRunId info}) "#{asyncInfoName info}" |]
                AsyncFinished info -> [i|#{show now} FINISHED (#{asyncInfoThreadId info}, #{asyncInfoRunId info}) "#{asyncInfoName info}"|]
          hPutStr h (line <> "\n")
          hFlush h
    loop `finally` do
      now <- getCurrentTime
      remaining <- getManagedAsyncInfos
      hPutStr h [i|\n#{show now} === Remaining managed asyncs: #{M.size remaining} ===\n|]
      forM_ (M.toList remaining) $ \(tid, info) ->
        hPutStr h [i|  #{tid}: #{asyncInfoName info} (runId: #{asyncInfoRunId info})\n|]
      hFlush h

-- | Write a tree of node IDs and labels to a file for cross-referencing with events.
writeTreeFile :: FilePath -> [RunNodeWithStatus context s l t] -> IO ()
writeTreeFile path rts =
  writeFile path $ unlines $ concatMap (renderTree 0) rts

renderTree :: Int -> RunNodeWithStatus context s l t -> [String]
renderTree depth node = line : children
  where
    c = runNodeCommon node
    indent = replicate (depth * 2) ' '
    label = runTreeLabel c
    nid = runTreeId c
    line = [i|#{indent}[#{nid}] #{label}|]
    children = case node of
      RunNodeIt {} -> []
      RunNodeIntroduce {runNodeChildrenAugmented} -> concatMap (renderTree (depth + 1)) runNodeChildrenAugmented
      RunNodeIntroduceWith {runNodeChildrenAugmented} -> concatMap (renderTree (depth + 1)) runNodeChildrenAugmented
      _ -> concatMap (renderTree (depth + 1)) (runNodeChildren node)
