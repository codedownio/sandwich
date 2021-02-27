{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Common
import Control.Monad.Reader
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
      pauseSeconds 1
      timeAction "Makes rice" $ do
        timeAction "Washes rice" $ pauseSeconds 1
        timeAction "Starts steamer" $ pauseSeconds 0.8
        timeAction "Serves rice" $ pauseSeconds 0.7

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions timingParallelDemo
