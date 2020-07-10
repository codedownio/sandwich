-- | Filter a spec tree

module Test.Sandwich.Interpreters.FilterTree (filterTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

-- | Filter a spec tree using a string
filterTree :: String -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
filterTree match (Free (Before l f subspec next))
  | l `matches` match = Free (Before l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Before l f x (filterTree match next))
filterTree match (Free (After l f subspec next))
  | l `matches` match = Free (After l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (After l f x (filterTree match next))
filterTree match (Free (Introduce l cl alloc cleanup subspec next))
  | l `matches` match = Free (Introduce l cl alloc cleanup subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Introduce l cl alloc cleanup x (filterTree match next))
filterTree match (Free (IntroduceWith l cl action subspec next))
  | l `matches` match = Free (IntroduceWith l cl action subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (IntroduceWith l cl action x (filterTree match next))
filterTree match (Free (Around l f subspec next))
  | l `matches` match = Free (Around l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Around l f x (filterTree match next))
filterTree match (Free (Describe l subspec next))
  | l `matches` match = Free (Describe l subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Describe l x (filterTree match next))
filterTree match (Free (Parallel subspec next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Parallel x (filterTree match next))
filterTree match (Free (It l ex next))
  | l `matches` match = Free (It l ex (filterTree match next))
  | otherwise = filterTree match (filterTree match next)
filterTree _ (Pure x) = Pure x


matches :: String -> String -> Bool
matches label match = match `L.isInfixOf` label
