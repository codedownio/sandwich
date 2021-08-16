{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The failure report formatter is like the print formatter, but it only shows failures.
--
-- Documentation can be found <https://codedownio.github.io/sandwich/docs/formatters/failure_report here>.

module Test.Sandwich.Formatters.FailureReport (
  defaultFailureReportFormatter
  , FailureReportFormatter

  -- * Options
  , failureReportUseColor
  , failureReportLogLevel
  , failureReportIncludeCallStacks
  , failureReportIndentSize
  , failureReportVisibilityThreshold
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Foldable
import Data.Function
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.String.Interpolate
import qualified Data.Text as T
import System.IO
import Test.Sandwich.Formatters.Print.Common
import Test.Sandwich.Formatters.Print.FailureReason
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


data FailureReportFormatter = FailureReportFormatter {
  failureReportUseColor :: Bool
  , failureReportLogLevel :: Maybe LogLevel
  , failureReportIncludeCallStacks :: Bool
  , failureReportIndentSize :: Int
  , failureReportVisibilityThreshold :: Int
  } deriving (Show)

defaultFailureReportFormatter :: FailureReportFormatter
defaultFailureReportFormatter = FailureReportFormatter {
  failureReportUseColor = True
  , failureReportLogLevel = Just LevelWarn
  , failureReportIncludeCallStacks = True
  , failureReportIndentSize = 4
  , failureReportVisibilityThreshold = 50
  }

instance Formatter FailureReportFormatter where
  formatterName _ = "failure-report-formatter"
  runFormatter _ _ _ _ = return ()
  finalizeFormatter = printFailureReport

printFailureReport :: (MonadIO m, MonadLogger m, MonadCatch m) => FailureReportFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
printFailureReport frf@(FailureReportFormatter {..}) rts _bc = do
  liftIO $ putStrLn [i|\n\nFailure report:|]

  let pf = PrintFormatter {
        printFormatterUseColor = failureReportUseColor
        , printFormatterLogLevel = failureReportLogLevel
        , printFormatterVisibilityThreshold = maxBound
        , printFormatterIncludeCallStacks = failureReportIncludeCallStacks
        , printFormatterIndentSize = failureReportIndentSize
        }

  let extractFromNode node = let RunNodeCommonWithStatus {..} = runNodeCommon node in (runTreeId, (T.pack runTreeLabel, runTreeVisibilityLevel))
  let idToLabel = M.fromList $ mconcat [extractValues extractFromNode node | node <- rts]

  liftIO $ runReaderT (mapM_ (runWithIndentation frf idToLabel) rts) (pf, 0, stdout)

runWithIndentation :: FailureReportFormatter -> M.Map Int (T.Text, Int) -> RunNode context -> ReaderT (PrintFormatter, Int, Handle) IO ()
runWithIndentation frf@(FailureReportFormatter {..}) idToLabel node = do
  let common@(RunNodeCommonWithStatus {..}) = runNodeCommon node

  case node of
    RunNodeIt {} -> return ()
    RunNodeIntroduce {..} -> forM_ runNodeChildrenAugmented (runWithIndentation frf idToLabel)
    RunNodeIntroduceWith {..} -> forM_ runNodeChildrenAugmented (runWithIndentation frf idToLabel)
    _ -> forM_ (runNodeChildren node) (runWithIndentation frf idToLabel)

  result <- liftIO $ waitForTree node

  -- Print the failure reason
  case result of
    Success -> return ()
    DryRun -> return ()
    Cancelled -> return ()
    Failure (ChildrenFailed {}) -> return ()
    Failure reason -> do
      p "\n"

      let ancestorIds = runTreeAncestors
      let ancestorNames = fmap (\k -> fromMaybe ("?", 0) $ M.lookup k idToLabel) ancestorIds
                        & Seq.filter (\(_, visibilityLevel) -> visibilityLevel <= failureReportVisibilityThreshold)
                        & fmap fst
      let label = T.unpack $ T.intercalate ", " (toList ancestorNames)

      case reason of
        Pending {} -> do
          pYellowLn label
        _ -> do
          -- TODO: get full list of ancestor labels joined on ", "
          pRedLn label
          withBumpIndent $ printFailureReason reason
          finishPrinting common result
