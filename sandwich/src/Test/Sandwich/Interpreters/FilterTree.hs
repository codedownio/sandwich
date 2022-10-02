
-- | Filter a spec tree using a string match

module Test.Sandwich.Interpreters.FilterTree (filterTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

filterTree :: Free (SpecCommand context m) () -> String -> Free (SpecCommand context m) ()
filterTree (Free (It'' loc no l ex next)) match
  | l `matches` match = Free (It'' loc no l ex (filterTree next match))
  | otherwise = filterTree (filterTree next match) match
filterTree (Free (Introduce'' loc no l cl alloc cleanup subspec next)) match
  | l `matches` match = Free (Introduce'' loc no l cl alloc cleanup subspec (filterTree next match))
  | otherwise = case filterTree subspec match of
      (Pure _) -> filterTree next match
      x -> Free (Introduce'' loc no l cl alloc cleanup x (filterTree next match))
filterTree (Free (IntroduceWith'' loc no l cl action subspec next)) match
  | l `matches` match = Free (IntroduceWith'' loc no l cl action subspec (filterTree next match))
  | otherwise = case filterTree subspec match of
      (Pure _) -> filterTree next match
      x -> Free (IntroduceWith'' loc no l cl action x (filterTree next match))
filterTree (Free (Parallel'' loc no subspec next)) match
  = case filterTree subspec match of
      (Pure _) -> filterTree next match
      x -> Free (Parallel'' loc no x (filterTree next match))
filterTree (Free x) match
  | label x `matches` match = Free (x { next = (filterTree (next x) match) })
  | otherwise = case filterTree (subspec x) match of
      (Pure _) -> filterTree (next x) match
      subspec' -> Free (x { subspec = subspec'
                          , next = (filterTree (next x) match) })
filterTree (Pure x) _ = Pure x


matches :: String -> String -> Bool
matches l match = match `L.isInfixOf` l
