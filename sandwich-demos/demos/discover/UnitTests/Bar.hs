
module UnitTests.Bar where

import Test.Sandwich


tests :: TopSpec'
tests = it "tests bar" $ do
  2 `shouldBe` 2
