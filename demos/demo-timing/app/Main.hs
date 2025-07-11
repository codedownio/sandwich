{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Test.Sandwich

timingDemo :: TopSpec
timingDemo = describe "Dinner tests" $ do
  it "Makes dinner" $ do
    pauseSeconds 1
    timeAction "Makes pasta" $ do
      timeAction "Heats water" $ pauseSeconds 1
      timeAction "Boils noodles" $ pauseSeconds 0.8
      timeAction "Decants noodles" $ pauseSeconds 0.7

  it "Cleans up" $ do
    pauseSeconds 1

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions timingDemo
