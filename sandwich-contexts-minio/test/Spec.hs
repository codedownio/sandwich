{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Spec where

import Test.Sandwich

#insert_test_imports


tests :: TopSpec
tests = $(getSpecFromFolder defaultGetSpecFromFolderOptions)

-- testsPooled :: PooledSpec
-- testsPooled = $(getSpecFromFolder $ defaultGetSpecFromFolderOptions {
--   getSpecCombiner = 'describeParallel
--   , getSpecIndividualSpecHooks = 'poolify
--   , getSpecWarnOnParseError = NoWarnOnParseError
--   })

-- main :: IO ()
-- main = pooledMain (return ()) testsPooled
