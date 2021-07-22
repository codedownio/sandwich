{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Common
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time.Clock
import System.Random
import Test.Sandwich


parallelNDemo :: TopSpec
parallelNDemo = describe "parallelN demo" $ do
  parallelN 4 $ do
    it "does thing 1" sleepRandom
    it "does thing 2" sleepRandom
    it "does thing 3" sleepRandom
    it "does thing 4" sleepRandom
    it "does thing 5" sleepRandom
    it "does thing 6" sleepRandom
    it "does thing 7" sleepRandom
    it "does thing 8" sleepRandom
    it "does thing 9" sleepRandom
    it "does thing 10" sleepRandom
    it "does thing 11" sleepRandom
    it "does thing 12" sleepRandom
    it "does thing 13" sleepRandom
    it "does thing 14" sleepRandom
    it "does thing 15" sleepRandom

sleepRandom :: ExampleM context ()
sleepRandom = liftIO $ do
  timeToSleep :: Int <- randomRIO (1500000, 3000000)
  threadDelay timeToSleep

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions parallelNDemo
