
module Main where

import Relude
import qualified Spec
import Test.Sandwich


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions $
  Spec.tests
