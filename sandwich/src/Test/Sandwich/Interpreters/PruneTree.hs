
-- | Prunes any nodes and their children in a spec tree that match any of a list of names

module Test.Sandwich.Interpreters.PruneTree (pruneTree) where

import Control.Monad.Free
import qualified Data.List as L
import Test.Sandwich.Types.Spec

pruneTree :: Free (SpecCommand context m) () -> String -> Free (SpecCommand context m) ()
pruneTree tree pruneLabel = go tree
  where
    go :: Free (SpecCommand context m) () -> Free (SpecCommand context m) ()
    go = \case
      (Free (It'' loc no label' ex next))
        | label' `doesNotMatch` pruneLabel -> Free (It'' loc no label' ex (go next))
        | otherwise -> go next
      (Free (Introduce'' loc no label' cl alloc cleanup subspec next))
        | label' `doesNotMatch` pruneLabel ->
          case go subspec of
            (Pure _) -> go next
            subspec' -> Free (Introduce'' loc no label' cl alloc cleanup subspec' (go next))
        | otherwise -> go next

      (Free (IntroduceWith'' loc no label' cl action subspec next))
        | label' `doesNotMatch` pruneLabel ->
          case go subspec of
            (Pure _) -> go next
            subspec' -> Free (IntroduceWith'' loc no label' cl action subspec' (go next))
        | otherwise -> go next
      (Free (Parallel'' loc no subspec next)) ->
        case go subspec of
          (Pure _) -> go next
          subspec' -> Free (Parallel'' loc no subspec' (go next))
      -- Before'', After'', Around'', Describe''
      (Free x)
        | label x `doesNotMatch` pruneLabel ->
          case go (subspec x) of
            (Pure _) -> go (next x)
            subspec' -> Free (x { subspec = subspec', next = go (next x) })
        | otherwise ->
          go (next x)
      pureM@(Pure _) -> pureM

doesNotMatch :: String -> String -> Bool
doesNotMatch label match = not $ match `L.isInfixOf` label
