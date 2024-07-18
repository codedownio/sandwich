
module Spec.Dummy where

import Relude
import Test.Sandwich


tests :: TopSpec
tests = do
  it "works" $ do
    2 `shouldBe` 2
