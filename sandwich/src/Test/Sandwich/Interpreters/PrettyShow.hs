{-# LANGUAGE FlexibleInstances #-}

module Test.Sandwich.Interpreters.PrettyShow (prettyShow) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec


-- | Pretty show a spec tree
prettyShow :: Free (SpecCommand context m) r -> String
prettyShow = prettyShow' 0

prettyShow' :: Int -> Free (SpecCommand context m) r -> String
prettyShow' indent (Free (Before'' _loc _no l _f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (After'' _loc _no l _f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Introduce'' _loc _no l _cl _alloc _cleanup subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (IntroduceWith'' _loc _no l _cl _action subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Around'' _loc _no l _f subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Describe'' _loc _no l subspec next)) = showNode indent l subspec next
prettyShow' indent (Free (Parallel'' _loc _no subspec next)) = showNode indent "parallel" subspec next
prettyShow' indent (Free (It'' _loc _no l _ex next)) = showNode indent l ((return ()) :: Free (SpecCommand () m) ()) next
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
