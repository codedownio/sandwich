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
  , printFormatterUseColor
  , printFormatterLogLevel
  , printFormatterIncludeCallStacks
  , printFormatterIndentSize
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate.IsString
import Data.Time.Clock
import System.IO
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Formatters.Print.CallStacks
import Test.Sandwich.Formatters.Print.FailureReason
import Test.Sandwich.Formatters.Print.Logs
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util

instance Formatter PrintFormatter where
  runFormatter = runApp
  formatterName _ = "print-formatter"

runApp :: (MonadIO m, MonadLogger m) => PrintFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
runApp pf@(PrintFormatter {..}) rts bc = liftIO $ do
  let total = countWhere isItBlock rts

  startTime <- getCurrentTime

  putStrLn "\n"
  putStrLn [i|Beginning suite of #{total} tests\n|]

  whenJust (baseContextRunRoot bc) $ \runRoot ->
    putStrLn [i|Run root: #{runRoot}\n|]

  runReaderT (mapM_ runWithIndentation rts) (pf, 1, stdout)
  putStrLn "\n"

  fixedTree <- atomically $ mapM fixRunTree rts
  let failed = countWhere isFailedItBlock fixedTree
  let pending = countWhere isPendingItBlock fixedTree

  endTime <- getCurrentTime
  let timeDiff = formatNominalDiffTime $ diffUTCTime endTime startTime

  if | failed == 0 -> putStr [i|All tests passed in #{timeDiff}.|]
     | otherwise -> putStr [i|#{failed} failed of #{total} in #{timeDiff}.|]
  case pending of
    0 -> putStrLn ""
    _ -> putStrLn [i| (#{pending} pending)|]


runWithIndentation :: RunNode context -> ReaderT (PrintFormatter, Int, Handle) IO ()
runWithIndentation node@(RunNodeIt {..}) = do
  includeCallStacks <- asks (printFormatterIncludeCallStacks . fst3)

  let RunNodeCommonWithStatus {..} = runNodeCommon

  result <- liftIO $ waitForTree node

  -- Print the main header
  case result of
    Success -> pGreenLn runTreeLabel
    (Failure (Pending _ _)) -> pYellowLn runTreeLabel
    (Failure reason) -> do
      pRedLn runTreeLabel
      withBumpIndent $ printFailureReason reason

  -- Print the callstack, if configured and present
  when includeCallStacks $ do
    case result of
      Failure (failureCallStack -> Just cs) -> do
        p "\n"
        withBumpIndent $ printCallStack cs
      _ -> return ()

  -- Print the logs, if configured
  printLogs runTreeLogs
runWithIndentation node = do
  includeCallStacks <- asks (printFormatterIncludeCallStacks . fst3)

  let RunNodeCommonWithStatus {..} = runNodeCommon node
  pin runTreeLabel
  case node of
    RunNodeIntroduce {..} -> withBumpIndent $ forM_ runNodeChildrenAugmented runWithIndentation
    RunNodeIntroduceWith {..} -> withBumpIndent $ forM_ runNodeChildrenAugmented runWithIndentation
    _ -> withBumpIndent $ forM_ (runNodeChildren node) runWithIndentation

  result <- liftIO $ waitForTree node

  -- Print the failure reason
  case result of
    Failure r -> withBumpIndent $ printFailureReason r
    Success -> return ()

  -- Print the callstack, if configured and present
  when includeCallStacks $ do
    case result of
      Failure (failureCallStack -> Just cs) -> do
        p "\n"
        withBumpIndent $ printCallStack cs
      _ -> return ()

  -- Print the logs, if configured
  printLogs runTreeLogs
