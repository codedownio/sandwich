module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Test.Sandwich

basic :: CoreSpec
basic = describe "Simple tests" $ do
  describe "Arithmetic" $ do
    it "adds" $ do
      (2 + 2) `shouldBe` 4
      (2 + 3) `shouldBe` 5

    it "subtracts" $ do
      warn "This might not be right..."
      (3 - 2) `shouldBe` 0

  describe "Strings" $
    it "concatenates" $
      ("abc" <> "def") `shouldBe` "abcdef"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
