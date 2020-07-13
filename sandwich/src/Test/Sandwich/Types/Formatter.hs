-- |

module Test.Sandwich.Types.Formatter where

import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

class Formatter f where
  runFormatter :: f -> [RunNode BaseContext] -> BaseContext -> IO ()
