{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module UnitTests where

import Control.Monad.Free
import Data.Function
import Data.Maybe
import Test.Sandwich
import Test.Sandwich.Nodes
import Test.Sandwich.Types.Spec


#insert_test_imports

tests :: TopSpec
tests = $(getSpecFromFolder 'describe)

-- main :: IO ()
-- main = runSandwichWithCommandLineArgs defaultOptions tests

-- | Gather all node options from a spec
gatherNodeOptions :: Free (SpecCommand context m) r -> [NodeOptions]
gatherNodeOptions (Free x@(It'' {})) = (nodeOptions x) : gatherNodeOptions (next x)
gatherNodeOptions (Free (IntroduceWith'' {..})) = nodeOptions : (gatherNodeOptions next <> gatherNodeOptions subspecAugmented)
gatherNodeOptions (Free (Introduce'' {..})) = nodeOptions : (gatherNodeOptions next <> gatherNodeOptions subspecAugmented)
gatherNodeOptions (Free x) = (nodeOptions x) : (gatherNodeOptions (next x) <> gatherNodeOptions (subspec x))
gatherNodeOptions (Pure _) = []


mainFunctions = gatherNodeOptions tests
              & fmap nodeOptionsMainFunction
              & catMaybes
