{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.Common.Count where

import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

countWhere :: forall s l t. (RunTreeWithStatus s l t -> Bool) -> [RunTreeWithStatus s l t] -> Int
countWhere p rts = sum $ fmap (countWhere' p) rts
  where
    countWhere' :: (RunTreeWithStatus s l t -> Bool) -> RunTreeWithStatus s l t -> Int
    countWhere' p rt@(RunTreeGroup {..}) =
      (if p rt then 1 else 0) + countWhere p runTreeChildren
    countWhere' picate rt@(RunTreeSingle {..}) = if picate rt then 1 else 0

isItBlock (RunTreeSingle {}) = True
isItBlock _ = False

isRunningItBlock (RunTreeSingle {runTreeStatus=(Running {})}) = True
isRunningItBlock _ = False

isSuccessItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=Success})}) = True
isSuccessItBlock _ = False

isPendingItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=(Failure (Pending {}))})}) = True
isPendingItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=(Failure {})})}) = False
isPendingItBlock _ = False

isFailedItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=(Failure (Pending {}))})}) = False
isFailedItBlock (RunTreeSingle {runTreeStatus=(Done {statusResult=(Failure {})})}) = True
isFailedItBlock _ = False

isDoneItBlock (RunTreeSingle {runTreeStatus=(Done {})}) = True
isDoneItBlock _ = False

isNotStartedItBlock (RunTreeSingle {runTreeStatus=(NotStarted {})}) = True
isNotStartedItBlock _ = False
