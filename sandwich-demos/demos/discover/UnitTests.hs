{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module UnitTests where

import Test.Sandwich

#insert_test_imports


tests :: TopSpec
tests = $(getSpecFromFolder 'describe)

-- main :: IO ()
-- main = runSandwichWithCommandLineArgs defaultOptions tests
