{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
-- |

module Test.Sandwich.RunTree where

import Control.Concurrent.STM
import GHC.Stack
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


extractValues :: (forall context. RunNodeWithStatus context s l t -> a) -> RunNodeWithStatus context s l t -> [a]
extractValues f node@(RunNodeIt {}) = [f node]
extractValues f node@(RunNodeIntroduce {runNodeChildrenAugmented}) = (f node) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues f node@(RunNodeIntroduceWith {runNodeChildrenAugmented}) = (f node) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues f node = (f node) : (concatMap (extractValues f) (runNodeChildren node))

isFailureStatus :: Status -> Bool
isFailureStatus (Done _ _ stat) = isFailure stat
isFailureStatus _ = False

getCommons :: RunNodeWithStatus context s l t -> [RunNodeCommonWithStatus s l t]
getCommons = extractValues runNodeCommon

fixRunTree :: RunNode context -> STM (RunNodeFixed context)
fixRunTree node@(runNodeCommon -> (RunNodeCommonWithStatus {..})) = do
  status <- readTVar runTreeStatus
  logs <- readTVar runTreeLogs
  toggled <- readTVar runTreeToggled
  open <- readTVar runTreeOpen

  let common' = RunNodeCommonWithStatus {
        runTreeStatus = status
        , runTreeLogs = logs
        , runTreeToggled = toggled
        , runTreeOpen = open
        , ..
        }

  case node of
    RunNodeBefore {..} -> do
      children <- mapM fixRunTree runNodeChildren
      return $ RunNodeBefore { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeAfter {..} -> do
      children <- mapM fixRunTree runNodeChildren
      return $ RunNodeAfter { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeIntroduce {..} -> do
      children <- mapM fixRunTree runNodeChildrenAugmented
      return $ RunNodeIntroduce { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
    RunNodeIntroduceWith {..} -> do
      children <- mapM fixRunTree runNodeChildrenAugmented
      return $ RunNodeIntroduceWith { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
    RunNodeAround {..} -> do
      children <- mapM fixRunTree runNodeChildren
      return $ RunNodeAround { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeDescribe {..} -> do
      children <- mapM fixRunTree runNodeChildren
      return $ RunNodeDescribe { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeParallel {..} -> do
      children <- mapM fixRunTree runNodeChildren
      return $ RunNodeParallel { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeIt {..} -> do
      return $ RunNodeIt { runNodeCommon=common', .. }

unFixRunTree :: RunNodeFixed context -> STM (RunNode context)
unFixRunTree node@(runNodeCommon -> (RunNodeCommonWithStatus {..})) = do
  status <- newTVar runTreeStatus
  logs <- newTVar runTreeLogs
  toggled <- newTVar runTreeToggled
  open <- newTVar runTreeOpen

  let common' = RunNodeCommonWithStatus {
        runTreeStatus = status
        , runTreeLogs = logs
        , runTreeToggled = toggled
        , runTreeOpen = open
        , ..
        }

  case node of
    RunNodeBefore {..} -> do
      children <- mapM unFixRunTree runNodeChildren
      return $ RunNodeBefore { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeAfter {..} -> do
      children <- mapM unFixRunTree runNodeChildren
      return $ RunNodeAfter { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeIntroduce {..} -> do
      children <- mapM unFixRunTree runNodeChildrenAugmented
      return $ RunNodeIntroduce { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
    RunNodeIntroduceWith {..} -> do
      children <- mapM unFixRunTree runNodeChildrenAugmented
      return $ RunNodeIntroduceWith { runNodeCommon=common', runNodeChildrenAugmented=children, .. }
    RunNodeAround {..} -> do
      children <- mapM unFixRunTree runNodeChildren
      return $ RunNodeAround { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeDescribe {..} -> do
      children <- mapM unFixRunTree runNodeChildren
      return $ RunNodeDescribe { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeParallel {..} -> do
      children <- mapM unFixRunTree runNodeChildren
      return $ RunNodeParallel { runNodeCommon=common', runNodeChildren=children, .. }
    RunNodeIt {..} -> do
      return $ RunNodeIt { runNodeCommon=common', .. }


getCallStackFromStatus :: Status -> Maybe CallStack
getCallStackFromStatus NotStarted {} = Nothing
getCallStackFromStatus Running {} = Nothing
getCallStackFromStatus Done {statusResult} = getCallStackFromResult statusResult

getCallStackFromResult :: Result -> Maybe CallStack
getCallStackFromResult (Success {}) = Nothing
getCallStackFromResult (Failure (Reason x _)) = x
getCallStackFromResult (Failure (ExpectedButGot x _ _)) = x
getCallStackFromResult (Failure (DidNotExpectButGot x _)) = x
getCallStackFromResult (Failure (Pending x _)) = x
getCallStackFromResult (Failure (GotException {})) = Nothing
getCallStackFromResult (Failure (GetContextException {})) = Nothing
getCallStackFromResult (Failure (GotAsyncException {})) = Nothing

isDone :: Status -> Bool
isDone (Done {}) = True
isDone _ = False

isRunning :: Status -> Bool
isRunning (Running {}) = True
isRunning _ = False
