{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Test.Sandwich.RunTree where

import Control.Concurrent.STM
import Data.Functor.Identity
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


extractValues :: (forall context. RunNodeWithStatus context v -> a) -> RunNodeWithStatus context v -> [a]
extractValues f node@(RunNodeIt {}) = [f node]
extractValues f node@(RunNodeIntroduce {runNodeChildrenAugmented}) = (f node) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues f node@(RunNodeIntroduceWith {runNodeChildrenAugmented}) = (f node) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues f node = (f node) : (concatMap (extractValues f) (runNodeChildren node))

extractValuesControlRecurse :: (forall context. RunNodeWithStatus context v -> (Bool, a)) -> RunNodeWithStatus context v -> [a]
extractValuesControlRecurse f node@(RunNodeIt {}) = [snd $ f node]
extractValuesControlRecurse f node@(RunNodeIntroduce {runNodeChildrenAugmented}) = case f node of
  (True, x) -> x : (concatMap (extractValuesControlRecurse f) runNodeChildrenAugmented)
  (False, x) -> [x]
extractValuesControlRecurse f node@(RunNodeIntroduceWith {runNodeChildrenAugmented}) = case f node of
  (True, x) -> x : (concatMap (extractValuesControlRecurse f) runNodeChildrenAugmented)
  (False, x) -> [x]
extractValuesControlRecurse f node = case f node of
  (True, x) -> x : (concatMap (extractValuesControlRecurse f) (runNodeChildren node))
  (False, x) -> [x]

getCommons :: RunNodeWithStatus context v -> [RunNodeCommonWithStatus v]
getCommons = extractValues runNodeCommon

fixRunTree :: RunNode context -> STM (RunNodeFixed context)
fixRunTree node@(runNodeCommon -> (RunNodeCommonWithStatus {..})) = do
  status <- readTVar runTreeStatus
  logs <- readTVar runTreeLogs
  toggled <- readTVar runTreeToggled
  open <- readTVar runTreeOpen

  let common' = RunNodeCommonWithStatus {
        runTreeStatus = Identity status
        , runTreeLogs = Identity logs
        , runTreeToggled = Identity toggled
        , runTreeOpen = Identity open
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
  status <- newTVar $ runIdentity runTreeStatus
  logs <- newTVar $ runIdentity runTreeLogs
  toggled <- newTVar $ runIdentity runTreeToggled
  open <- newTVar $ runIdentity runTreeOpen

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


isDone :: Status -> Bool
isDone (Done {}) = True
isDone _ = False

isRunning :: Status -> Bool
isRunning (Running {}) = True
isRunning _ = False

isFailureStatus :: Status -> Bool
isFailureStatus (Done _ _ stat) = isFailure stat
isFailureStatus _ = False

isFailure :: Result -> Bool
isFailure (Failure (Pending {})) = False
isFailure (Failure {}) = True
isFailure _ = False

isPending :: Result -> Bool
isPending (Failure (Pending {})) = True
isPending _ = False

whenFailure :: (Monad m) => Result -> (FailureReason -> m ()) -> m ()
whenFailure (Failure (Pending {})) _ = return ()
whenFailure (Failure reason) action = action reason
whenFailure _ _ = return ()
