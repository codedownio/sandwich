{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Time.Clock
import Test.Sandwich
import UnitTests


discoverDemo :: CoreSpec
discoverDemo = describe "Discover" $ do
  UnitTests.tests

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions discoverDemo
