{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Filter (
  filterRunTree
  ) where

import Data.Function
import Test.Sandwich.Types.RunTree

filterRunTree :: Bool -> [RunTreeFixed] -> [RunTreeFixed]
filterRunTree showContextManagers rtsFixed = rtsFixed
  & if showContextManagers then id else filterContextManagers

filterContextManagers :: [RunTreeFixed] -> [RunTreeFixed]
filterContextManagers = mconcat . fmap filterContextManagersSingle

filterContextManagersSingle :: RunTreeFixed -> [RunTreeFixed]
filterContextManagersSingle rt@(RunTreeGroup {runTreeIsContextManager=False, ..}) = [rt { runTreeChildren = filterContextManagers runTreeChildren }]
filterContextManagersSingle (RunTreeGroup {runTreeIsContextManager=True, ..}) = filterContextManagers runTreeChildren
filterContextManagersSingle rt@(RunTreeSingle {}) = [rt]
