{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Common
import Data.Time.Clock
import Test.Sandwich

timingParallelDemo :: TopSpec
timingParallelDemo = parallel $ do
  withTimingProfile "italian" $
    describe "Makes Italian dinner" $
      pauseSeconds 1
      timeAction "Makes pasta" $ do
        timeAction "Heats water" $ pauseSeconds 1
        timeAction "Boils noodles" $ pauseSeconds 0.8
        timeAction "Decants noodles" $ pauseSeconds 0.7

  withTimingProfile "chinese" $
    describe "Makes Chinese dinner" $
      pauseSeconds 1
      timeAction "Makes rice" $ do
        timeAction "Washes rice" $ pauseSeconds 1
        timeAction "Starts steamer" $ pauseSeconds 0.8
        timeAction "Serves rice" $ pauseSeconds 0.7

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions timingDemo
