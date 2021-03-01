{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}

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
import Test.Sandwich.Formatters.Print.Common
import Test.Sandwich.Formatters.Print.FailureReason
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util


instance Formatter PrintFormatter where
  formatterName _ = "print-formatter"
  runFormatter = runApp
  finalizeFormatter _ _ _ = return ()

runApp :: (MonadIO m, MonadLogger m) => PrintFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
runApp pf@(PrintFormatter {..}) rts bc = liftIO $ do
  let total = countWhere isItBlock rts

  startTime <- getCurrentTime

  putStrLn "\n"
  putStrLn [i|Beginning suite of #{total} tests\n|]

  whenJust (baseContextRunRoot bc) $ \runRoot ->
    putStrLn [i|Run root: #{runRoot}\n|]

  runReaderT (mapM_ runWithIndentation rts) (pf, 2, stdout)
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
  let common@(RunNodeCommonWithStatus {..}) = runNodeCommon

  result <- liftIO $ waitForTree node

  -- Print the main header
  case result of
    Success -> pGreenLn runTreeLabel
    (Failure (Pending _ _)) -> pYellowLn runTreeLabel
    (Failure reason) -> do
      pRedLn runTreeLabel
      withBumpIndent $ printFailureReason reason

  finishPrinting common result
runWithIndentation node = do
  let common@(RunNodeCommonWithStatus {..}) = runNodeCommon node

  (PrintFormatter {..}, _, _) <- ask

  childPrintFn <- case runTreeVisibilityLevel <= printFormatterVisibilityThreshold of
    True -> do
      pin runTreeLabel
      return withBumpIndent
    False -> return id

  case node of
    RunNodeIntroduce {..} -> childPrintFn $ forM_ runNodeChildrenAugmented runWithIndentation
    RunNodeIntroduceWith {..} -> childPrintFn $ forM_ runNodeChildrenAugmented runWithIndentation
    _ -> childPrintFn $ forM_ (runNodeChildren node) runWithIndentation

  result <- liftIO $ waitForTree node

  -- Print the failure reason
  case runTreeVisibilityLevel <= printFormatterVisibilityThreshold of
    True -> do
      case result of
        Failure r -> withBumpIndent $ printFailureReason r
        Success -> return ()
      finishPrinting common result
    False -> return () -- TODO: print failure info even though node should be hidden?
