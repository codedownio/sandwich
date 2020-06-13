-- |

module Test.Sandwich.Types.Formatter where

import Test.Sandwich.Types.RunTree

class Formatter f where
  runFormatter :: f -> [RunTree] -> IO ()
