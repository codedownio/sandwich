-- | Internal functionality exposed for sibling libraries such as sandwich-webdriver to use. Should not be used otherwise.

module Test.Sandwich.Internal (
  Spec
  , SpecWith
  , SpecFree
  , HasBaseContext
  , HasLabel
  , LabelValue(..)
  , (:>)(..)
  , ExampleM
  , ExampleT(..)
  ) where

import Test.Sandwich.Types.Spec
