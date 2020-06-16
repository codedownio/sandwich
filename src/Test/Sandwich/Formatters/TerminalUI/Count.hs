{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Count where

import Test.Sandwich.Types.Example
import Test.Sandwich.Types.RunTree

countWhere :: (RunTreeFixed -> Bool) -> [RunTreeFixed] -> Int
countWhere p rts = sum $ fmap (countWhere' p) rts
  where
    countWhere' :: (RunTreeFixed -> Bool) -> RunTreeFixed -> Int
    countWhere' p rt@(RunTreeGroup {..}) =
      (if p rt then 1 else 0) + countWhere p runTreeChildren
    countWhere' picate rt@(RunTreeSingle {..}) = if picate rt then 1 else 0

isItBlock (RunTreeSingle {}) = True
isItBlock _ = False

isRunningItBlock (RunTreeSingle {runTreeStatus=(Running {})}) = True
isRunningItBlock _ = False

isSuccessItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=Success})}) = True
isSuccessItBlock _ = False

isPendingItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=(Pending {})})}) = True
isPendingItBlock _ = False

isFailedItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=(Failure {})})}) = True
isFailedItBlock _ = False

isDoneItBlock (RunTreeSingle {runTreeStatus=(Done {})}) = True
isDoneItBlock _ = False

isNotStartedItBlock (RunTreeSingle {runTreeStatus=(NotStarted {})}) = True
isNotStartedItBlock _ = False
