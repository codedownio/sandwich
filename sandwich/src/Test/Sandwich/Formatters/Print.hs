{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.Print (
  defaultPrintFormatter
  ) where

import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.String.Interpolate.IsString
import Graphics.Vty
import System.Console.ANSI
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Print.CallStacks
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.FailureReason
import Test.Sandwich.Formatters.Print.Logs
import Test.Sandwich.Formatters.Print.PrintPretty
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util
import Text.Show.Pretty as P

instance Formatter PrintFormatter where
  runFormatter = runApp

runApp :: PrintFormatter -> [RunTree] -> IO ()
runApp pf@(PrintFormatter {..}) rts = do
  let total = countWhere isItBlock rts

  putStrLn "\n"
  putStrLn [i|Beginning suite of #{total} tests\n|]

  runReaderT (mapM_ runWithIndentation rts) (pf, 1)
  putStrLn "\n"

  fixedTree <- atomically $ mapM fixRunTree rts
  let succeeded = countWhere isSuccessItBlock fixedTree
  let failed = countWhere isFailedItBlock fixedTree
  let pending = countWhere isPendingItBlock fixedTree
  let total = countWhere isItBlock fixedTree

  if | failed == 0 -> putStr [i|All tests passed!|]
     | otherwise -> putStr [i|#{failed} failed of #{total}.|]
  case pending of
    0 -> putStrLn ""
    _ -> putStrLn [i| (#{pending} pending)|]


runWithIndentation :: RunTree -> ReaderT (PrintFormatter, Int) IO ()
runWithIndentation (RunTreeGroup {..}) = do
  pin runTreeLabel
  withBumpIndent $ forM_ runTreeChildren runWithIndentation
runWithIndentation (RunTreeSingle {..}) = do
  -- Get settings
  includeCallStacks <- asks (printFormatterIncludeCallStacks . fst)
  includeLogs <- asks (printFormatterIncludeLogs . fst)

  -- currentLogsVar <- liftIO $ newTVarIO mempty
  -- logReaderAsync <- async $ forever $ do
  --   logsToHandle <- liftIO $ atomically $ do
  --     currentLogs <- readTVar currentLogsVar
  --     newLogs <- readTVar runTreeLogs
  --     when (newLogs == currentLogs) retry

  --     writeTVar currentLogsVar newLogs
  --     return $ Seq.drop (L.length currentLogs) newLogs
  --   withBumpIndent $
  --     forM logsToHandle printLogEntry

  liftIO $ wait runTreeAsync
  -- liftIO $ wait logReaderAsync

  -- Print the main header
  (liftIO $ readTVarIO runTreeStatus) >>= \case
    NotStarted -> return ()
    Running {} -> return ()
    Done {statusResult=Success} -> pGreenLn runTreeLabel
    Done {statusResult=(Failure (Pending maybeCallStack maybeMessage))} -> pYellowLn runTreeLabel
    Done {statusResult=(Failure reason)} -> do
      pRedLn runTreeLabel
      withBumpIndent $ printFailureReason reason

  -- Print the callstack, if configured and present
  when includeCallStacks $ do
    (liftIO $ readTVarIO runTreeStatus) >>= \case
      Done {statusResult=(Failure (failureCallStack -> Just cs))} ->
        withBumpIndent $ printCallStack cs
      _ -> return ()

  -- Print the logs, if configured
  when includeLogs $ do
    logEntries <- liftIO $ readTVarIO runTreeLogs
    withBumpIndent $
      forM_ logEntries printLogEntry
