{-# LANGUAGE FlexibleInstances #-}
-- |

module Test.Sandwich.Interpreters.PrettyShow (prettyShow) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

-- | Pretty show a spec tree
prettyShow :: Free (SpecCommand context) r -> String
prettyShow = prettyShow' 0

prettyShow' :: Int -> Free (SpecCommand context) r -> String
prettyShow' indent (Free (Before l f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (After l f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Introduce l cl alloc cleanup subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Around l f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Describe l subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (DescribeParallel l subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (It l ex next)) = showNode indent l ((return ()) :: Free (SpecCommand ()) ()) next
prettyShow' _ (Pure _) = ""

-- * Util
  
indentSize :: Int
indentSize = 2

showNode :: Int -> String -> Free (SpecCommand c) r -> Free (SpecCommand c') r' -> String
showNode indent label subspec next = L.intercalate "\n" $ filter (/= "") [
  (L.replicate indent ' ') <> label
  , prettyShow' (indent + indentSize) subspec
  , prettyShow' indent next
  ]
