
module UnitTests.Foo where

import Test.Sandwich


tests :: TopSpec'
tests = it "tests foo" $ do
  2 `shouldBe` 2
