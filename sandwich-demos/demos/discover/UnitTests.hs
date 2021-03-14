{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}

module UnitTests where

import Data.List
import Test.Sandwich

#insert_test_imports

tests :: TopSpec
tests = describe "Unit tests" $ foldl' (>>) (return ()) #test_imports_list

-- main = putStrLn "HI"
