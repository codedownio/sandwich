{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Filter (
  filterRunTree
  ) where

import Data.Function
import Test.Sandwich.Types.RunTree

filterRunTree :: Int -> [RunNodeWithStatus context s l t] -> [RunNodeWithStatus context s l t]
filterRunTree visibilityThreshold rtsFixed = rtsFixed
  & id
  -- (filterVisibilityThreshold visibilityThreshold)

-- filterVisibilityThreshold :: Int -> [RunNodeWithStatus context s l t] -> [RunNodeWithStatus context s l t]
-- filterVisibilityThreshold thresh = mconcat . fmap (filterVisibilityThresholdSingle thresh)

-- filterVisibilityThresholdSingle :: Int -> RunNodeWithStatus context s l t -> [RunNodeWithStatus context s l t]
-- filterVisibilityThresholdSingle thresh node | thresh < runTreeVisibilityLevel (runNodeCommon node) = case node of
--                                                 RunNodeIt {} -> []
--                                                 RunNodeIntroduce {..} -> filterVisibilityThreshold thresh runNodeChildrenAugmented

-- filterVisibilityThresholdSingle thresh node@(RunNodeIntroduce {..}) = [RunNodeIntroduce { runNodeChildrenAugmented = filterVisibilityThreshold thresh runNodeChildrenAugmented, .. }]
-- filterVisibilityThresholdSingle thresh node = [node { runNodeChildren = filterVisibilityThreshold thresh (runNodeChildren node) }]
--   = [rt { runTreeChildren = filterVisibilityThreshold runTreeChildren }]
-- filterVisibilityThresholdSingle thresh (RunTreeGroup {runTreeIsContextManager=True, ..}) = filterVisibilityThreshold runTreeChildren
-- filterVisibilityThresholdSingle thresh rt@(RunTreeSingle {}) = [rt]
