{-# LANGUAGE RankNTypes #-}

module Test.Sandwich.Formatters.Socket.Commands (
  handleCommand
  ) where

import Control.Concurrent.STM
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String.Interpolate
import Data.Time
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Read (readMaybe)


-- | Handle a single command and return the response text.
-- The response does NOT include the trailing ".\n" terminator; the caller adds that.
handleCommand :: [RunNode BaseContext] -> UTCTime -> String -> IO String
handleCommand rts now cmd = case words cmd of
  ["help"] -> return helpText
  ["status"] -> cmdStatus rts
  ["active"] -> cmdActive rts now
  ["failures"] -> cmdFailures rts
  ["pending"] -> cmdPending rts
  ["tree"] -> cmdTree rts
  ["node", idStr] -> case readMaybe idStr of
    Just nid -> cmdNode rts nid
    Nothing -> return [i|Error: invalid node id "#{idStr}"|]
  ["logs", idStr] -> case readMaybe idStr of
    Just nid -> cmdLogs rts nid
    Nothing -> return [i|Error: invalid node id "#{idStr}"|]
  [] -> return ""
  (c:_) -> return [i|Unknown command: #{c}\nType "help" for available commands.|]

helpText :: String
helpText = unlines
  [ "Available commands:"
  , "  help        - Show this help"
  , "  status      - Summary counts: total, running, succeeded, failed, pending, not started"
  , "  active      - List currently running nodes"
  , "  failures    - List failed nodes with failure reason"
  , "  pending     - List pending nodes"
  , "  tree        - Full tree with indented status"
  , "  node <id>   - Detail for a specific node"
  , "  logs <id>   - Show logs for a specific node"
  , "  stream-logs      - Stream all logs live (disconnect to stop)"
  , "  stream-events    - Stream node lifecycle events (started/done) live"
  , "  stream-rts-stats - Stream GHC RTS memory stats every 1s (needs +RTS -T)"
  ]

-- | Snapshot the tree atomically
snapshot :: [RunNode BaseContext] -> IO [RunNodeFixed BaseContext]
snapshot rts = atomically $ mapM fixRunTree rts

-- * Commands

cmdStatus :: [RunNode BaseContext] -> IO String
cmdStatus rts = do
  fixed <- snapshot rts
  let total = countWhere isItBlock fixed
      running = countWhere isRunningItBlock fixed
      succeeded = countWhere isSuccessItBlock fixed
      failed = countWhere isFailedItBlock fixed
      pend = countWhere isPendingItBlock fixed
      notStarted = countWhere isNotStartedItBlock fixed
  return $ unlines
    [ [i|total:       #{total}|]
    , [i|running:     #{running}|]
    , [i|succeeded:   #{succeeded}|]
    , [i|failed:      #{failed}|]
    , [i|pending:     #{pend}|]
    , [i|not started: #{notStarted}|]
    ]

cmdActive :: [RunNode BaseContext] -> UTCTime -> IO String
cmdActive rts now = do
  fixed <- snapshot rts
  let nodes = concatMap (extractValues getActiveInfo) fixed
      activeNodes = [x | Just x <- nodes]
  if null activeNodes
    then return "No nodes currently running."
    else return $ unlines [formatActive n | n <- activeNodes]
  where
    getActiveInfo :: RunNodeWithStatus ctx Status (Seq LogEntry) Bool -> Maybe (String, Int, NominalDiffTime)
    getActiveInfo node = case runTreeStatus (runNodeCommon node) of
      Running {statusStartTime} ->
        let c = runNodeCommon node
        in Just (runTreeLabel c, runTreeId c, diffUTCTime now statusStartTime)
      _ -> Nothing

    formatActive (label, nid, elapsed) =
      let elapsedStr = formatNominalDiffTime elapsed
      in [i|  [#{nid}] #{label} (#{elapsedStr})|]

cmdFailures :: [RunNode BaseContext] -> IO String
cmdFailures rts = do
  fixed <- snapshot rts
  let nodes = concatMap (extractValues getFailureInfo) fixed
      failedNodes = [x | Just x <- nodes]
  if null failedNodes
    then return "No failures."
    else return $ unlines [formatFailure n | n <- failedNodes]
  where
    getFailureInfo :: RunNodeWithStatus ctx Status (Seq LogEntry) Bool -> Maybe (String, Int, FailureReason)
    getFailureInfo node = case runTreeStatus (runNodeCommon node) of
      Done {statusResult = Failure reason@(Pending {})} -> Nothing
      Done {statusResult = Failure reason} ->
        let c = runNodeCommon node
        in Just (runTreeLabel c, runTreeId c, reason)
      _ -> Nothing

    formatFailure (label, nid, reason) =
      [i|  [#{nid}] #{label}: #{showFailureReason reason}|]

cmdPending :: [RunNode BaseContext] -> IO String
cmdPending rts = do
  fixed <- snapshot rts
  let nodes = concatMap (extractValues getPendingInfo) fixed
      pendingNodes = [x | Just x <- nodes]
  if null pendingNodes
    then return "No pending nodes."
    else return $ unlines [formatPendingNode n | n <- pendingNodes]
  where
    getPendingInfo :: RunNodeWithStatus ctx Status (Seq LogEntry) Bool -> Maybe (String, Int, Maybe String)
    getPendingInfo node = case runTreeStatus (runNodeCommon node) of
      Done {statusResult = Failure (Pending {failurePendingMessage})} ->
        let c = runNodeCommon node
        in Just (runTreeLabel c, runTreeId c, failurePendingMessage)
      _ -> Nothing

    formatPendingNode (label, nid, msg) = case msg of
      Just m -> [i|  [#{nid}] #{label}: #{m}|]
      Nothing -> [i|  [#{nid}] #{label}|]

cmdTree :: [RunNode BaseContext] -> IO String
cmdTree rts = do
  fixed <- snapshot rts
  return $ unlines $ concatMap (renderTree 0) fixed

renderTree :: Int -> RunNodeWithStatus context Status (Seq LogEntry) Bool -> [String]
renderTree depth node =
  let c = runNodeCommon node
      indent = replicate (depth * 2) ' '
      statusStr = showStatusBrief (runTreeStatus c)
      label = runTreeLabel c
      nid = runTreeId c
      line = [i|#{indent}[#{statusStr}] [#{nid}] #{label}|]
      children = case node of
        RunNodeIt {} -> []
        RunNodeIntroduce {runNodeChildrenAugmented} -> concatMap (renderTree (depth + 1)) runNodeChildrenAugmented
        RunNodeIntroduceWith {runNodeChildrenAugmented} -> concatMap (renderTree (depth + 1)) runNodeChildrenAugmented
        _ -> concatMap (renderTree (depth + 1)) (runNodeChildren node)
  in line : children

cmdNode :: [RunNode BaseContext] -> Int -> IO String
cmdNode rts nid = do
  fixed <- snapshot rts
  let allCommons = concatMap (extractValues (runNodeCommon)) fixed
      match = [c | c <- allCommons, runTreeId c == nid]
  case match of
    [] -> return [i|Error: no node with id #{nid}|]
    (c:_) -> return $ unlines
      [ [i|id:     #{runTreeId c}|]
      , [i|label:  #{runTreeLabel c}|]
      , [i|status: #{showStatusDetail (runTreeStatus c)}|]
      , [i|folder: #{maybe "(none)" id (runTreeFolder c)}|]
      ]

cmdLogs :: [RunNode BaseContext] -> Int -> IO String
cmdLogs rts nid = do
  fixed <- snapshot rts
  let allNodes = concatMap (extractValues (\n -> (runNodeCommon n))) fixed
      match = [c | c <- allNodes, runTreeId c == nid]
  case match of
    [] -> return [i|Error: no node with id #{nid}|]
    (c:_) -> do
      let logs = runTreeLogs c
      if Seq.null logs
        then return "(no logs)"
        else return $ unlines [showLogEntry e | e <- toList logs]

-- * Formatting helpers

showStatusBrief :: Status -> String
showStatusBrief NotStarted = "NOT STARTED"
showStatusBrief (Running {}) = "RUNNING"
showStatusBrief (Done {statusResult = Success}) = "OK"
showStatusBrief (Done {statusResult = Failure (Pending {})}) = "PENDING"
showStatusBrief (Done {statusResult = Failure _}) = "FAIL"
showStatusBrief (Done {statusResult = DryRun}) = "DRY RUN"
showStatusBrief (Done {statusResult = Cancelled}) = "CANCELLED"

showStatusDetail :: Status -> String
showStatusDetail NotStarted = "not started"
showStatusDetail (Running {statusStartTime}) = [i|running (started #{show statusStartTime})|]
showStatusDetail (Done {statusStartTime, statusEndTime, statusResult}) =
  let elapsed = formatNominalDiffTime (diffUTCTime statusEndTime statusStartTime)
  in [i|#{showResultBrief statusResult} (#{elapsed})|]

showResultBrief :: Result -> String
showResultBrief Success = "succeeded"
showResultBrief (Failure (Pending {})) = "pending"
showResultBrief (Failure _) = "failed"
showResultBrief DryRun = "dry run"
showResultBrief Cancelled = "cancelled"

showFailureReason :: FailureReason -> String
showFailureReason (Reason {failureReason}) = failureReason
showFailureReason (ExpectedButGot {failureValue1, failureValue2}) =
  [i|Expected #{show failureValue1} but got #{show failureValue2}|]
showFailureReason (DidNotExpectButGot {failureValue1}) =
  [i|Did not expect #{show failureValue1}|]
showFailureReason (GotException {failureMessage, failureException}) =
  case failureMessage of
    Just msg -> [i|#{msg}: #{show failureException}|]
    Nothing -> show failureException
showFailureReason (Pending {failurePendingMessage}) =
  maybe "pending" id failurePendingMessage
showFailureReason (GetContextException {failureException}) =
  [i|Context exception: #{show failureException}|]
showFailureReason (GotAsyncException {failureMessage, failureAsyncException}) =
  case failureMessage of
    Just msg -> [i|#{msg}: #{show failureAsyncException}|]
    Nothing -> show failureAsyncException
showFailureReason (ChildrenFailed {failureNumChildren}) =
  [i|#{failureNumChildren} children failed|]
showFailureReason (RawImage {failureFallback}) = failureFallback

showLogEntry :: LogEntry -> String
showLogEntry (LogEntry {logEntryTime, logEntryLevel, logEntryStr}) =
  let levelStr :: String
      levelStr = case logEntryLevel of
        LevelDebug -> "DEBUG"
        LevelInfo -> "INFO"
        LevelWarn -> "WARN"
        LevelError -> "ERROR"
        LevelOther t -> show t
      msgStr = BS8.unpack logEntryStr
  in [i|#{show logEntryTime} [#{levelStr}] #{msgStr}|]
