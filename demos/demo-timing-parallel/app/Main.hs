{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Data.Time.Clock
import Test.Sandwich


timingParallelDemo :: TopSpec
timingParallelDemo = parallel $ do
  withTimingProfile "italian" $
    it "Makes Italian dinner" $ do
      pauseSeconds 1
      timeAction "Makes pasta" $ do
        timeAction "Heats water" $ pauseSeconds 1
        timeAction "Boils noodles" $ pauseSeconds 0.8
        timeAction "Decants noodles" $ pauseSeconds 0.7

  withTimingProfile "chinese" $
    it "Makes Chinese dinner" $ do
      pauseSeconds 0.1
      timeAction "Makes rice" $ do
        timeAction "Cooks rice" $ pauseSeconds 0.5
        timeAction "Serves rice" $ pauseSeconds 0.2
      pauseSeconds 0.3

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions timingParallelDemo
