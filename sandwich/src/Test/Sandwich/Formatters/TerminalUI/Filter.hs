{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Filter (
  filterRunTree
  ) where

import Data.Function
import Test.Sandwich.Types.RunTree

filterRunTree :: Bool -> [RunNodeWithStatus context a l t] -> [RunNodeWithStatus context a l t]
filterRunTree showContextManagers rtsFixed = rtsFixed
  & id
  -- & if showContextManagers then id else filterContextManagers

-- filterContextManagers :: [RunNodeWithStatus a l t] -> [RunNodeWithStatus a l t]
-- filterContextManagers = mconcat . fmap filterContextManagersSingle

-- filterContextManagersSingle :: RunNodeWithStatus a l t -> [RunNodeWithStatus a l t]
-- filterContextManagersSingle rt@(RunTreeGroup {runTreeIsContextManager=False, ..}) = [rt { runTreeChildren = filterContextManagers runTreeChildren }]
-- filterContextManagersSingle (RunTreeGroup {runTreeIsContextManager=True, ..}) = filterContextManagers runTreeChildren
-- filterContextManagersSingle rt@(RunTreeSingle {}) = [rt]
