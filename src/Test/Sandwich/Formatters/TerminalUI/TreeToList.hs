{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.TreeToList (
  treeToVector
  ) where

import qualified Data.Vector as Vec
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.RunTree

treeToVector :: [(RunTree, RunTreeFixed)] -> Vec.Vector MainListElem
treeToVector = runTreesToList' 0

runTreesToList' :: Int -> [(RunTree, RunTreeFixed)] -> Vec.Vector MainListElem
runTreesToList' indent rts = mconcat $ fmap (runTreeToList' indent) rts

runTreeToList' :: Int -> (RunTree, RunTreeFixed) -> Vec.Vector MainListElem
runTreeToList' indent (node, fixedNode@(RunTreeGroup {})) = elem `Vec.cons` (runTreesToList' (indent + 1)
                                                                            (zip (runTreeChildren node) (runTreeChildren fixedNode)))
  where elem = MainListElem {
          label = runTreeLabel fixedNode
          , depth = indent
          , toggled = runTreeToggled fixedNode
          , status = runTreeStatus fixedNode
          , logs = runTreeLogs fixedNode
          , isContextManager = runTreeIsContextManager fixedNode
          , node = node
          }
runTreeToList' indent (node, (RunTreeSingle {..})) = Vec.singleton elem
  where elem = MainListElem {
          label = runTreeLabel
          , depth = indent
          , toggled = runTreeToggled
          , status = runTreeStatus
          , logs = runTreeLogs
          , isContextManager = False
          , node = node
          }
