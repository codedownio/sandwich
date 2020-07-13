{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.TreeToList (
  treeToVector
  ) where

import qualified Data.Vector as Vec
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.RunTree

treeToVector :: [(RunNode context, RunNodeFixed context)] -> Vec.Vector MainListElem
treeToVector = runTreesToList' 0

runTreesToList' :: Int -> [(RunNode context, RunNodeFixed context)] -> Vec.Vector MainListElem
runTreesToList' indent rts = mconcat $ fmap (runTreeToList' indent) rts

runTreeToList' :: Int -> (RunNode context, RunNodeFixed context) -> Vec.Vector MainListElem
runTreeToList' indent (node, fixedNode) = case fixedNode of
  RunNodeIt {} -> Vec.singleton elem
  _ -> elem `Vec.cons` (runTreesToList' (indent + 1) (zip (runNodeChildren node) (runNodeChildren fixedNode)))
  where elem = MainListElem {
          label = runTreeLabel $ runNodeCommon fixedNode
          , depth = indent
          , toggled = runTreeToggled $ runNodeCommon fixedNode
          , status = runTreeStatus $ runNodeCommon fixedNode
          , logs = runTreeLogs $ runNodeCommon fixedNode
          , isContextManager = False -- TODO
          , visibilityLevel = runTreeVisibilityLevel $ runNodeCommon fixedNode
          , folderPath = runTreeFolder $ runNodeCommon fixedNode
          , node = runNodeCommon node
          }
