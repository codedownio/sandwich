{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.TreeToList (
  treeToBrickList
  , treeToVector
  ) where

import qualified Brick.Widgets.List as L
import qualified Data.List as L
import qualified Data.Vector as Vec
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.RunTree

treeToBrickList :: [RunTreeFixed] -> L.GenericList () Vec.Vector MainListElem
treeToBrickList runTree = L.list () (treeToVector runTree) 1

treeToVector :: [RunTreeFixed] -> Vec.Vector MainListElem
treeToVector = runTreesToList' 0

runTreesToList' :: Int -> [RunTreeFixed] -> Vec.Vector MainListElem
runTreesToList' indent rts = mconcat $ fmap (runTreeToList' indent) rts

runTreeToList' :: Int -> RunTreeFixed -> Vec.Vector MainListElem
runTreeToList' indent (RunTreeGroup {..}) = elem `Vec.cons` (runTreesToList' (indent + 1) runTreeChildren)
  where elem = MainListElem {
          label = runTreeLabel
          , depth = indent
          , folded = False
          , status = runTreeStatus
          , logs = runTreeLogs
          , isContextManager = runTreeIsContextManager
          }
runTreeToList' indent (RunTreeSingle {..}) = Vec.singleton elem
  where elem = MainListElem {
          label = runTreeLabel
          , depth = indent
          , folded = False
          , status = runTreeStatus
          , logs = runTreeLogs
          , isContextManager = False
          }
