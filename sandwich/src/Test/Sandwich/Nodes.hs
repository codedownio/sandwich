
-- | This module exports the special "primed" versions of the test tree nodes. These primed versions accept an additional options argument, which allows you to do things like control the visibility thresholds of the nodes.

module Test.Sandwich.Nodes (
  -- * Basic nodes
  it'
  , describe'
  , parallel'

  -- * Node options
  , NodeOptions
  , defaultNodeOptions
  , nodeOptionsVisibilityThreshold
  , nodeOptionsCreateFolder
  , nodeOptionsRecordTime

  -- * Context manager nodes
  , introduce'
  , introduceWith'
  , before'
  , beforeEach'
  , after'
  , afterEach'
  , around'
  , aroundEach'

  ) where

import Test.Sandwich.Types.Spec
