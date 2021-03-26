
module UnitTests.NestedNoFile.Baz (tests, main) where

import Test.Sandwich


tests :: TopSpec'
tests = it "tests baz nested no file" $ do
  2 `shouldBe` 2

main = runSandwichWithCommandLineArgs defaultOptions tests
