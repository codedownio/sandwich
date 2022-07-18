module Main where

import Test.Sandwich
import Test.Sandwich.Golden


goldenDemo :: TopSpec
goldenDemo = describe "Simple tests" $ do
  describe "myStringFunc" $
    golden $ goldenString "myStringFunc" (myStringFunc ())

myStringFunc _ = "foo"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions goldenDemo
