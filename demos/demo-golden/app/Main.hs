module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Test.Sandwich
import Test.Sandwich.Golden


golden :: TopSpec
golden = describe "Simple tests" $ do
  describe "myTextFunc" $
    golden' $ defaultGolden "myTextFunc" (myTextFunc ())

myTextFunc _ = "asdf"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions golden
