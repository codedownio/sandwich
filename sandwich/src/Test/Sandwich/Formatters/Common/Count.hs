{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |

module Test.Sandwich.Formatters.Common.Count where

import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

countWhere :: (forall context. RunNodeWithStatus context s l t -> Bool) -> [RunNodeWithStatus context s l t] -> Int
countWhere p rts = sum $ fmap (countWhere' p) rts
  where
    countWhere' :: (forall context. RunNodeWithStatus context s l t -> Bool) -> RunNodeWithStatus context s l t -> Int
    countWhere' p rt@(RunNodeIt {..}) = if p rt then 1 else 0
    countWhere' p rt@(RunNodeIntroduce {..}) = (if p rt then 1 else 0) + countWhere p runNodeChildrenAugmented
    countWhere' p rt@(RunNodeIntroduceWith {..}) = (if p rt then 1 else 0) + countWhere p runNodeChildrenAugmented
    countWhere' p rt = (if p rt then 1 else 0) + countWhere p (runNodeChildren rt)

isItBlock (RunNodeIt {}) = True
isItBlock _ = False

isRunningItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Running {})})}) = True
isRunningItBlock _ = False

isSuccessItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=Success})})}) = True
isSuccessItBlock _ = False

isPendingItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=(Failure (Pending {}))})})}) = True
isPendingItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=(Failure {})})})}) = False
isPendingItBlock _ = False

isFailedItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=(Failure (Pending {}))})})}) = False
isFailedItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=(Failure {})})})}) = True
isFailedItBlock _ = False

isFailedBlock (runNodeCommon -> (RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=(Failure (Pending {}))})})) = False
isFailedBlock (runNodeCommon -> (RunNodeCommonWithStatus {runTreeStatus=(Done {statusResult=(Failure {})})})) = True
isFailedBlock _ = False

isDoneItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(Done {})})}) = True
isDoneItBlock _ = False

isNotStartedItBlock (RunNodeIt {runNodeCommon=(RunNodeCommonWithStatus {runTreeStatus=(NotStarted {})})}) = True
isNotStartedItBlock _ = False
