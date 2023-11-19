{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (Main.main) where

import Test.Sandwich

import qualified SeleniumTests
import qualified UnitTests


discoverDemo :: TopSpec
discoverDemo = describe "Discover" $ do
  UnitTests.tests
  SeleniumTests.tests

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions discoverDemo
