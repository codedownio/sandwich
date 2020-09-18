-- | Filter a spec tree

module Test.Sandwich.Interpreters.FilterTree (filterTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

-- | Filter a spec tree using a string
filterTree :: String -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
filterTree match (Free (Before'' loc no l f subspec next))
  | l `matches` match = Free (Before'' loc no l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Before'' loc no l f x (filterTree match next))
filterTree match (Free (After'' loc no l f subspec next))
  | l `matches` match = Free (After'' loc no l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (After'' loc no l f x (filterTree match next))
filterTree match (Free (Introduce'' loc no l cl alloc cleanup subspec next))
  | l `matches` match = Free (Introduce'' loc no l cl alloc cleanup subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Introduce'' loc no l cl alloc cleanup x (filterTree match next))
filterTree match (Free (IntroduceWith'' loc no l cl action subspec next))
  | l `matches` match = Free (IntroduceWith'' loc no l cl action subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (IntroduceWith'' loc no l cl action x (filterTree match next))
filterTree match (Free (Around'' loc no l f subspec next))
  | l `matches` match = Free (Around'' loc no l f subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Around'' loc no l f x (filterTree match next))
filterTree match (Free (Describe'' loc no l subspec next))
  | l `matches` match = Free (Describe'' loc no l subspec (filterTree match next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Describe'' loc no l x (filterTree match next))
filterTree match (Free (Parallel'' loc no subspec next))
  | otherwise = case filterTree match subspec of
      (Pure _) -> filterTree match next
      x -> Free (Parallel'' loc no x (filterTree match next))
filterTree match (Free (It'' loc no l ex next))
  | l `matches` match = Free (It'' loc no l ex (filterTree match next))
  | otherwise = filterTree match (filterTree match next)
filterTree _ (Pure x) = Pure x


matches :: String -> String -> Bool
matches label match = match `L.isInfixOf` label
