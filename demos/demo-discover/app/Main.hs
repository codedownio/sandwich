{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import Test.Sandwich

import qualified SeleniumTests
import qualified UnitTests


discoverDemo :: TopSpecWithOptions
discoverDemo = describe "Discover" $ do
  UnitTests.tests
  SeleniumTests.spec

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions discoverDemo
