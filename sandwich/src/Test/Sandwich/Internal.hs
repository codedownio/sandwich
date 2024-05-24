-- | Internal functionality exposed for debugging or for sibling libraries such as sandwich-webdriver to use. Should not be used otherwise.

module Test.Sandwich.Internal (
  Spec
  , SpecFree
  , SpecCommand
  , HasBaseContext
  , HasLabel
  , LabelValue(..)
  , (:>)(..)
  , ExampleM
  , ExampleT(..)

  -- For tests
  , RunNodeWithStatus(..)
  , RunNodeFixed
  , RunNode
  , RunNodeCommonWithStatus(..)
  , extractValues
  , extractValuesControlRecurse
  , getCommons
  , cancelNode
  , Status(..)
  , fixRunTree
  , waitForTree
  , SomeAsyncExceptionWithEq(..)
  , logEntryStr

  -- For debugging
  , getRunTree
  , getRunTree'
  , printRunTree

  , module Test.Sandwich.Internal.Formatters
  , module Test.Sandwich.Internal.Running
  ) where

import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

import Test.Sandwich.Internal.Formatters
import Test.Sandwich.Internal.Inspection
import Test.Sandwich.Internal.Running
