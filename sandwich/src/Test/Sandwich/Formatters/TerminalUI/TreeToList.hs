{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.TreeToList (
  treeToVector
  ) where

import qualified Data.Vector as Vec
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.RunTree

-- TODO: this is horrible, how to fix?
-- Problem is the something like (Introduce {}, Introduce {}) might have two different types
-- due to the introduce label on the constructor...
import Unsafe.Coerce

treeToVector :: [(RunNode context, RunNodeFixed context)] -> Vec.Vector MainListElem
treeToVector = runTreesToList' 0

runTreesToList' :: Int -> [(RunNode context, RunNodeFixed context)] -> Vec.Vector MainListElem
runTreesToList' indent rts = mconcat $ fmap (runTreeToList' indent) rts

runTreeToList' :: Int -> (RunNode context, RunNodeFixed context) -> Vec.Vector MainListElem
runTreeToList' indent (node@(RunNodeIt {}), fixedNode@(RunNodeIt {})) =
  Vec.singleton (makeElem indent fixedNode node)
runTreeToList' indent (node@(RunNodeIntroduce {runNodeChildrenAugmented=childrenVariable}), fixedNode@(RunNodeIntroduce {runNodeChildrenAugmented=childrenFixed})) =
  (makeElem indent fixedNode node) `Vec.cons` (runTreesToList' (indent + 1) (zip childrenVariable (unsafeCoerce childrenFixed)))
runTreeToList' indent (node@(RunNodeIntroduceWith {runNodeChildrenAugmented=childrenVariable}), fixedNode@(RunNodeIntroduceWith {runNodeChildrenAugmented=childrenFixed})) =
  (makeElem indent fixedNode node) `Vec.cons` (runTreesToList' (indent + 1) (zip childrenVariable (unsafeCoerce childrenFixed)))
runTreeToList' indent (node, fixedNode) =
  (makeElem indent fixedNode node) `Vec.cons` (runTreesToList' (indent + 1) (zip (runNodeChildren node) (runNodeChildren fixedNode)))

makeElem indent fixedNode node = MainListElem {
  label = runTreeLabel $ runNodeCommon fixedNode
  , depth = indent
  , toggled = runTreeToggled $ runNodeCommon fixedNode
  , status = runTreeStatus $ runNodeCommon fixedNode
  , logs = runTreeLogs $ runNodeCommon fixedNode
  , isContextManager = False -- TODO
  , visibilityLevel = runTreeVisibilityLevel $ runNodeCommon fixedNode
  , folderPath = runTreeFolder $ runNodeCommon fixedNode
  , node = runNodeCommon node
  , runNode = SomeRunNode node
  }
