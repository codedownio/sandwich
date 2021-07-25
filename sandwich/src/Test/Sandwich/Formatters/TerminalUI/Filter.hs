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
import Data.Functor.Identity
import qualified Data.List as L
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree

filterRunTree :: Int -> [RunNodeFixed context] -> [RunNodeFixed context]
filterRunTree visibilityThreshold rtsFixed = rtsFixed
  & fmap (mapCommon (hideIfThresholdAbove visibilityThreshold))
  & fmap hideClosed

mapCommon :: (RunNodeCommonWithStatus v -> RunNodeCommonWithStatus v) -> RunNodeWithStatus context v -> RunNodeWithStatus context v
mapCommon f node@(RunNodeIt {}) = node { runNodeCommon = f (runNodeCommon node) }
mapCommon f (RunNodeIntroduce {..}) = RunNodeIntroduce { runNodeCommon = f runNodeCommon
                                                       , runNodeChildrenAugmented = fmap (mapCommon f) runNodeChildrenAugmented
                                                       , .. }
mapCommon f (RunNodeIntroduceWith {..}) = RunNodeIntroduceWith { runNodeCommon = f runNodeCommon
                                                               , runNodeChildrenAugmented = fmap (mapCommon f) runNodeChildrenAugmented
                                                               , .. }
mapCommon f node = node { runNodeCommon = f (runNodeCommon node)
                        , runNodeChildren = fmap (mapCommon f) (runNodeChildren node) }


hideIfThresholdAbove :: Int -> RunNodeCommonFixed -> RunNodeCommonFixed
hideIfThresholdAbove visibilityThreshold node@(RunNodeCommonWithStatus {..}) =
  if | runTreeVisibilityLevel <= visibilityThreshold -> node { runTreeVisible = True }
     | otherwise -> node { runTreeVisible = False
                         , runTreeOpen = Identity True -- Must be open so children have a chance to be seen
                         }

markClosed :: RunNodeCommonWithStatus Identity -> RunNodeCommonWithStatus Identity
markClosed node@(RunNodeCommonWithStatus {..}) = node { runTreeVisible = False }

hideClosed :: RunNodeWithStatus context Identity -> RunNodeWithStatus context Identity
hideClosed node@(RunNodeIt {}) = node
hideClosed (RunNodeIntroduce {..})
  | runIdentity $ runTreeOpen runNodeCommon = RunNodeIntroduce { runNodeChildrenAugmented = fmap hideClosed runNodeChildrenAugmented, .. }
  | otherwise = RunNodeIntroduce { runNodeChildrenAugmented = fmap (mapCommon markClosed) runNodeChildrenAugmented, .. }
hideClosed (RunNodeIntroduceWith {..})
  | runIdentity $ runTreeOpen runNodeCommon = RunNodeIntroduceWith { runNodeChildrenAugmented = fmap hideClosed runNodeChildrenAugmented, .. }
  | otherwise = RunNodeIntroduceWith { runNodeChildrenAugmented = fmap (mapCommon markClosed) runNodeChildrenAugmented, .. }
hideClosed node
  | runIdentity $ runTreeOpen (runNodeCommon node) = node { runNodeChildren = fmap hideClosed (runNodeChildren node) }
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
      , toggled = runIdentity runTreeToggled
      , open = runIdentity runTreeOpen
      , status = runIdentity runTreeStatus
      , logs = runIdentity runTreeLogs
      , visibilityLevel = runTreeVisibilityLevel
      , folderPath = runTreeFolder
      , node = common
      , ident = runTreeId
      }

getCommonsWithVisibleDepth' :: RunNodeWithStatus context v -> Reader Int [(RunNodeCommonWithStatus v, Int)]
getCommonsWithVisibleDepth' node@(RunNodeIt {}) = ask >>= \vd -> return [(runNodeCommon node, vd)]
getCommonsWithVisibleDepth' (RunNodeIntroduce {..}) = do
  let context = if runTreeVisible runNodeCommon then (local (+1)) else id
  rest <- context $ (mconcat <$>) $ forM runNodeChildrenAugmented getCommonsWithVisibleDepth'
  ask >>= \vd -> return ((runNodeCommon, vd) : rest)
getCommonsWithVisibleDepth' (RunNodeIntroduceWith {..}) = do
  let context = if runTreeVisible runNodeCommon then (local (+1)) else id
  rest <- context $ (mconcat <$>) $ forM runNodeChildrenAugmented getCommonsWithVisibleDepth'
  ask >>= \vd -> return ((runNodeCommon, vd) : rest)
getCommonsWithVisibleDepth' node = do
  let context = if runTreeVisible (runNodeCommon node) then (local (+1)) else id
  rest <- context $ (mconcat <$>) $ forM (runNodeChildren node) getCommonsWithVisibleDepth'
  ask >>= \vd -> return ((runNodeCommon node, vd) : rest)
