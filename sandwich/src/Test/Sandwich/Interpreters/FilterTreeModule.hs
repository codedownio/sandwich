
-- | Filter a spec tree to match a module

module Test.Sandwich.Interpreters.FilterTreeModule (filterTreeToModule) where

import Control.Monad.Free
import Test.Sandwich.Types.Spec


filterTreeToModule :: String -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
filterTreeToModule match (Free (Before'' loc no l f subspec next))
  | no `matches` match = Free (Before'' loc no l f subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (Before'' loc no l f x (filterTreeToModule match next))
filterTreeToModule match (Free (After'' loc no l f subspec next))
  | no `matches` match = Free (After'' loc no l f subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (After'' loc no l f x (filterTreeToModule match next))
filterTreeToModule match (Free (Introduce'' loc no l cl alloc cleanup subspec next))
  | no `matches` match = Free (Introduce'' loc no l cl alloc cleanup subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (Introduce'' loc no l cl alloc cleanup x (filterTreeToModule match next))
filterTreeToModule match (Free (IntroduceWith'' loc no l cl action subspec next))
  | no `matches` match = Free (IntroduceWith'' loc no l cl action subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (IntroduceWith'' loc no l cl action x (filterTreeToModule match next))
filterTreeToModule match (Free (Around'' loc no l f subspec next))
  | no `matches` match = Free (Around'' loc no l f subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (Around'' loc no l f x (filterTreeToModule match next))
filterTreeToModule match (Free (Describe'' loc no l subspec next))
  | no `matches` match = Free (Describe'' loc no l subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (Describe'' loc no l x (filterTreeToModule match next))
filterTreeToModule match (Free (Parallel'' loc no subspec next))
  | no `matches` match = Free (Parallel'' loc no subspec (filterTreeToModule match next))
  | otherwise = case filterTreeToModule match subspec of
      (Pure _) -> filterTreeToModule match next
      x -> Free (Parallel'' loc no x (filterTreeToModule match next))
filterTreeToModule match (Free (It'' loc no l ex next))
  | no `matches` match = Free (It'' loc no l ex (filterTreeToModule match next))
  | otherwise = filterTreeToModule match (filterTreeToModule match next)
filterTreeToModule _ (Pure x) = Pure x


matches :: NodeOptions -> String -> Bool
matches (NodeOptions {nodeOptionsModuleInfo=(Just (NodeModuleInfo {..}))}) match = nodeModuleInfoModuleName == match
matches _ _ = False
