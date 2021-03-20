
-- | Filter a spec tree using a string match

module Test.Sandwich.Interpreters.FilterTree (filterTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

filterTree :: String -> Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
filterTree match (Free (It'' loc no l ex next))
  | l `matches` match = Free (It'' loc no l ex (filterTree match next))
  | otherwise = filterTree match (filterTree match next)
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
filterTree match (Free x)
  | label x `matches` match = Free (x { next = (filterTree match (next x)) })
  | otherwise = case filterTree match (subspec x) of
      (Pure _) -> filterTree match (next x)
      subspec' -> Free (x { subspec = subspec'
                          , next = (filterTree match (next x)) })
filterTree _ (Pure x) = Pure x


matches :: String -> String -> Bool
matches l match = match `L.isInfixOf` l
