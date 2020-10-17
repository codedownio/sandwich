{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.FailureReport (
  defaultFailureReportFormatter
  , failureReportUseColor
  , failureReportLogLevel
  , failureReportIncludeCallStacks
  , failureReportIndentSize
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate.IsString
import System.IO
import Test.Sandwich.Formatters.Print.Common
import Test.Sandwich.Formatters.Print.FailureReason
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


data FailureReportFormatter = FailureReportFormatter {
  failureReportUseColor :: Bool
  , failureReportLogLevel :: Maybe LogLevel
  , failureReportIncludeCallStacks :: Bool
  , failureReportIndentSize :: Int
  }

defaultFailureReportFormatter :: FailureReportFormatter
defaultFailureReportFormatter = FailureReportFormatter {
  failureReportUseColor = True
  , failureReportLogLevel = Just LevelWarn
  , failureReportIncludeCallStacks = True
  , failureReportIndentSize = 4
  }

instance Formatter FailureReportFormatter where
  formatterName _ = "failure-report-formatter"
  runFormatter _ _ _ = return ()
  finalize = printFailureReport

printFailureReport :: (MonadIO m, MonadLogger m, MonadCatch m) => FailureReportFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
printFailureReport (FailureReportFormatter {..}) rts baseContext = do
  liftIO $ putStrLn [i|\n\nFailure report:|]

  let pf = PrintFormatter {
        printFormatterUseColor = failureReportUseColor
        , printFormatterLogLevel = failureReportLogLevel
        , printFormatterIncludeCallStacks = failureReportIncludeCallStacks
        , printFormatterIndentSize = failureReportIndentSize
        }

  liftIO $ runReaderT (mapM_ runWithIndentation rts) (pf, 0, stdout)

runWithIndentation :: RunNode context -> ReaderT (PrintFormatter, Int, Handle) IO ()
runWithIndentation node = do
  let common@(RunNodeCommonWithStatus {..}) = runNodeCommon node

  case node of
    RunNodeIt {} -> return ()
    RunNodeIntroduce {..} -> forM_ runNodeChildrenAugmented runWithIndentation
    RunNodeIntroduceWith {..} -> forM_ runNodeChildrenAugmented runWithIndentation
    _ -> forM_ (runNodeChildren node) runWithIndentation

  result <- liftIO $ waitForTree node

  -- Print the failure reason
  case result of
    Success -> return ()
    Failure reason -> do
      p "\n"

      case reason of
        Pending {} -> do
          pYellowLn runTreeLabel
        _ -> do
          pRedLn runTreeLabel
          withBumpIndent $ printFailureReason reason
          finishPrinting common result
