
module UnitTests.NestedNoFile.Baz where

import Test.Sandwich


tests :: TopSpec
tests = it "tests baz nested no file" $ do
  2 `shouldBe` 2
