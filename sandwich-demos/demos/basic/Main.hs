module Main where

import Test.Sandwich

basic :: TopSpec
basic = describe "Simple tests" $ do
  describe "Arithmetic" $ do
    it "tests addition" $ do
      (2 + 2) `shouldBe` 4
      (2 + 3) `shouldBe` 5

    it "tests subtraction" $
      (3 - 2) `shouldBe` 1

  describe "Strings" $
    it "concatenates strings" $
      ("abc" <> "def") `shouldBe` "abcdef"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
