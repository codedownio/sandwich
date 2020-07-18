{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Filter (
  filterRunTree
  , treeToList
  ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Function
import qualified Data.List as L
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree

filterRunTree :: Int -> [RunNodeWithStatus context s l Bool] -> [RunNodeWithStatus context s l Bool]
filterRunTree visibilityThreshold rtsFixed = rtsFixed
  & fmap (mapCommon (hideIfThresholdAbove visibilityThreshold))
  & fmap hideClosed

mapCommon :: (RunNodeCommonWithStatus s l t -> RunNodeCommonWithStatus s l t) -> RunNodeWithStatus context s l t -> RunNodeWithStatus context s l t
mapCommon f node@(RunNodeIt {}) = node { runNodeCommon = f (runNodeCommon node) }
mapCommon f (RunNodeIntroduce {..}) = RunNodeIntroduce { runNodeCommon = f runNodeCommon
                                                       , runNodeChildrenAugmented = fmap (mapCommon f) runNodeChildrenAugmented
                                                       , .. }
mapCommon f (RunNodeIntroduceWith {..}) = RunNodeIntroduceWith { runNodeCommon = f runNodeCommon
                                                               , runNodeChildrenAugmented = fmap (mapCommon f) runNodeChildrenAugmented
                                                               , .. }
mapCommon f node = node { runNodeCommon = f (runNodeCommon node)
                        , runNodeChildren = fmap (mapCommon f) (runNodeChildren node) }


hideIfThresholdAbove :: Int -> RunNodeCommonWithStatus s l t -> RunNodeCommonWithStatus s l t
hideIfThresholdAbove visibilityThreshold node@(RunNodeCommonWithStatus {..}) =
  if | runTreeVisibilityLevel <= visibilityThreshold -> node { runTreeVisible = True }
     | otherwise -> node { runTreeVisible = False }

markClosed :: RunNodeCommonWithStatus s l Bool -> RunNodeCommonWithStatus s l Bool
markClosed node@(RunNodeCommonWithStatus {..}) = node { runTreeVisible = False }

hideClosed :: RunNodeWithStatus context s l Bool -> RunNodeWithStatus context s l Bool
hideClosed node@(RunNodeIt {}) = node
hideClosed (RunNodeIntroduce {..})
  | runTreeOpen runNodeCommon = RunNodeIntroduce { runNodeChildrenAugmented = fmap hideClosed runNodeChildrenAugmented, .. }
  | otherwise = RunNodeIntroduce { runNodeChildrenAugmented = fmap (mapCommon markClosed) runNodeChildrenAugmented, .. }
hideClosed (RunNodeIntroduceWith {..})
  | runTreeOpen runNodeCommon = RunNodeIntroduceWith { runNodeChildrenAugmented = fmap hideClosed runNodeChildrenAugmented, .. }
  | otherwise = RunNodeIntroduceWith { runNodeChildrenAugmented = fmap (mapCommon markClosed) runNodeChildrenAugmented, .. }
hideClosed node
  | runTreeOpen (runNodeCommon node) = node { runNodeChildren = fmap hideClosed (runNodeChildren node) }
  | otherwise = node { runNodeChildren = fmap (mapCommon markClosed) (runNodeChildren node) }


treeToList :: (RunNodeFixed context, RunNode context) -> [MainListElem]
treeToList (nodeFixed, node) = L.zip (runReader (getCommonsWithVisibleDepth' nodeFixed) 0) (getCommons node)
  & L.filter (isVisible . fst . fst)
  & fmap commonToMainListElem
  where

    isVisible :: RunNodeCommonFixed -> Bool
    isVisible (RunNodeCommonWithStatus {..}) = runTreeVisible

    commonToMainListElem :: ((RunNodeCommonFixed, Int), RunNodeCommon) -> MainListElem
    commonToMainListElem ((RunNodeCommonWithStatus {..}, depth), common) = MainListElem {
      label = runTreeLabel
      , depth = depth
      , toggled = runTreeToggled
      , open = runTreeOpen
      , status = runTreeStatus
      , logs = runTreeLogs
      , visibilityLevel = runTreeVisibilityLevel
      , folderPath = runTreeFolder
      , node = common
      , ident = runTreeId
      }

getCommonsWithVisibleDepth' :: RunNodeWithStatus context s l t -> Reader Int [(RunNodeCommonWithStatus s l t, Int)]
getCommonsWithVisibleDepth' node@(RunNodeIt {}) = ask >>= \vd -> return [(runNodeCommon node, vd)]
getCommonsWithVisibleDepth' node@(RunNodeIntroduce {..}) = do
  let context = if runTreeVisible runNodeCommon then (local (+1)) else id
  rest <- context $ (mconcat <$>) $ forM runNodeChildrenAugmented getCommonsWithVisibleDepth'
  ask >>= \vd -> return ((runNodeCommon, vd) : rest)
getCommonsWithVisibleDepth' node@(RunNodeIntroduceWith {..}) = do
  let context = if runTreeVisible runNodeCommon then (local (+1)) else id
  rest <- context $ (mconcat <$>) $ forM runNodeChildrenAugmented getCommonsWithVisibleDepth'
  ask >>= \vd -> return ((runNodeCommon, vd) : rest)
getCommonsWithVisibleDepth' node = do
  let context = if runTreeVisible (runNodeCommon node) then (local (+1)) else id
  rest <- context $ (mconcat <$>) $ forM (runNodeChildren node) getCommonsWithVisibleDepth'
  ask >>= \vd -> return ((runNodeCommon node, vd) : rest)
