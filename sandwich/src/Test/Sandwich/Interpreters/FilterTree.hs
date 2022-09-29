
-- | Filter a spec tree using a string match

module Test.Sandwich.Interpreters.FilterTree (filterTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

filterTree :: [String] -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
filterTree matches (Free (It'' loc no l ex next))
  | all (`L.isInfixOf` l) matches = Free (It'' loc no l ex (filterTree matches next))
  | otherwise = filterTree matches (filterTree matches next)
filterTree matches (Free (Introduce'' loc no l cl alloc cleanup subspec next))
  | all (`L.isInfixOf` l) matches = Free (Introduce'' loc no l cl alloc cleanup subspec (filterTree matches next))
  | otherwise = case filterTree matches subspec of
      (Pure _) -> filterTree matches next
      x -> Free (Introduce'' loc no l cl alloc cleanup x (filterTree matches next))
filterTree matches (Free (IntroduceWith'' loc no l cl action subspec next))
  | all (`L.isInfixOf` l) matches = Free (IntroduceWith'' loc no l cl action subspec (filterTree matches next))
  | otherwise = case filterTree matches subspec of
      (Pure _) -> filterTree matches next
      x -> Free (IntroduceWith'' loc no l cl action x (filterTree matches next))
filterTree matches (Free (Parallel'' loc no subspec next)) =
  case filterTree matches subspec of
    (Pure _) -> filterTree matches next
    x -> Free (Parallel'' loc no x (filterTree matches next))
filterTree matches (Free x)
  | all (`L.isInfixOf` label x) matches = Free (x { next = (filterTree matches (next x)) })
  | otherwise = case filterTree matches (subspec x) of
      (Pure _) -> filterTree matches (next x)
      subspec' -> Free (x { subspec = subspec'
                          , next = (filterTree matches (next x)) })
filterTree _ (Pure x) = Pure x
