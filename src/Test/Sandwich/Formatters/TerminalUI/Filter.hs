{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Filter (
  filterRunTree
  ) where

import Data.Function
import Test.Sandwich.Types.RunTree

filterRunTree :: Bool -> [RunTreeWithStatus a l t] -> [RunTreeWithStatus a l t]
filterRunTree showContextManagers rtsFixed = rtsFixed
  & if showContextManagers then id else filterContextManagers

filterContextManagers :: [RunTreeWithStatus a l t] -> [RunTreeWithStatus a l t]
filterContextManagers = mconcat . fmap filterContextManagersSingle

filterContextManagersSingle :: RunTreeWithStatus a l t -> [RunTreeWithStatus a l t]
filterContextManagersSingle rt@(RunTreeGroup {runTreeIsContextManager=False, ..}) = [rt { runTreeChildren = filterContextManagers runTreeChildren }]
filterContextManagersSingle (RunTreeGroup {runTreeIsContextManager=True, ..}) = filterContextManagers runTreeChildren
filterContextManagersSingle rt@(RunTreeSingle {}) = [rt]
