
module UnitTests.NestedWithFile.Baz where

import Test.Sandwich


tests :: TopSpec
tests = it "tests baz nested with file" $ do
  2 `shouldBe` 2

main = runSandwichWithCommandLineArgs [] defaultOptions tests
