module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Test.Sandwich

basic :: TopSpec
basic = describe "Simple tests" $ do
  describe "Arithmetic" $ do
    it "tests addition" $ do
      (2 + 2) `shouldBe` 4
      (2 + 3) `shouldBe` 5

    it "tests subtraction" $ do
      (3 - 2) `shouldBe` 1
      liftIO $ threadDelay 3000000
      warn "TODO: make sure this test is correct"

  describe "Strings" $
    it "concatenates strings" $
      ("abc" <> "def") `shouldBe` "abcdef"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
