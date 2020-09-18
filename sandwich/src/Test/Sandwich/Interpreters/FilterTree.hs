-- | Filter a spec tree

module Test.Sandwich.Interpreters.FilterTree (filterTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

-- | Filter a spec tree using a string
filterTree :: String -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
filterTree match (Free (Before' no loc l f subspec next))
  | l `matches` match = Free (Before' no loc l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Before' no loc l f x (filterTree match next))
filterTree match (Free (After' no loc l f subspec next))
  | l `matches` match = Free (After' no loc l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (After' no loc l f x (filterTree match next))
filterTree match (Free (Introduce' no loc l cl alloc cleanup subspec next))
  | l `matches` match = Free (Introduce' no loc l cl alloc cleanup subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Introduce' no loc l cl alloc cleanup x (filterTree match next))
filterTree match (Free (IntroduceWith' no loc l cl action subspec next))
  | l `matches` match = Free (IntroduceWith' no loc l cl action subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (IntroduceWith' no loc l cl action x (filterTree match next))
filterTree match (Free (Around' no loc l f subspec next))
  | l `matches` match = Free (Around' no loc l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Around' no loc l f x (filterTree match next))
filterTree match (Free (Describe' no loc l subspec next))
  | l `matches` match = Free (Describe' no loc l subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Describe' no loc l x (filterTree match next))
filterTree match (Free (Parallel' no loc subspec next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Parallel' no loc x (filterTree match next))
filterTree match (Free (It' no loc l ex next))
  | l `matches` match = Free (It' no loc l ex (filterTree match next))
  | otherwise = filterTree match (filterTree match next)
filterTree _ (Pure x) = Pure x


matches :: String -> String -> Bool
matches label match = match `L.isInfixOf` label
