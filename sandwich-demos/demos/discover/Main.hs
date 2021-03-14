{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Control.Monad
import Data.String.Interpolate
import Data.Time.Clock
import Test.Sandwich
import UnitTests


discoverDemo :: TopSpec
discoverDemo = describe "Discover" $ do
  UnitTests.tests

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions discoverDemo
