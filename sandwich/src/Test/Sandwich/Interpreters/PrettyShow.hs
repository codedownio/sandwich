{-# LANGUAGE FlexibleInstances #-}
-- |

module Test.Sandwich.Interpreters.PrettyShow (prettyShow) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

-- | Pretty show a spec tree
prettyShow :: Free (SpecCommand context m) r -> String
prettyShow = prettyShow' 0

prettyShow' :: Int -> Free (SpecCommand context m) r -> String
prettyShow' indent (Free (Before' no l f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (After' no l f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Introduce' no l cl alloc cleanup subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (IntroduceWith' no l cl action subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Around' no l f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Describe' no l subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Parallel' no subspec next)) = showNode indent "parallel" subspec next
prettyShow' indent (Free (It' no l ex next)) = showNode indent l ((return ()) :: Free (SpecCommand () m) ()) next
prettyShow' _ (Pure _) = ""

-- * Util

indentSize :: Int
indentSize = 2

showNode :: Int -> String -> Free (SpecCommand c m) r -> Free (SpecCommand c' m) r' -> String
showNode indent label subspec next = L.intercalate "\n" $ filter (/= "") [
  (L.replicate indent ' ') <> label
  , prettyShow' (indent + indentSize) subspec
  , prettyShow' indent next
  ]
