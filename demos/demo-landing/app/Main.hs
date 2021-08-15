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
import Test.Sandwich.Formatters.Print


landingDemo :: TopSpec
landingDemo = describe "Arithmetic tests" $ parallel $ do
  withTimingProfile "Addition" $
    describe "Addition" $ do
      it "basic addition" $ do
        (2 + 2) `shouldBe` 4
        sleepRandom

      it "adding zero" $ do
        (2 + 0) `shouldBe` 2
        sleepRandom

      it "adding one" $ do
        sleepRandom
        warn "Having some trouble getting this test to pass..."
        (0 + 1) `shouldBe` 0

  withTimingProfile "Multiplication" $
    describe "Multiplication" $ do
      it "small numbers" $ do
        (2 * 3) `shouldBe` 6
        sleepRandom

      it "multiplying by zero" $ do
        (2 * 0) `shouldBe` 0
        sleepRandom

      it "multiplying by one" $ do
        (2 * 1) `shouldBe` 2
        sleepRandom

      it "squaring" $ do
        (2 * 2) `shouldBe` 4
        sleepRandom


sleepRandom :: ExampleM context ()
sleepRandom = liftIO $ do
  timeToSleep :: Int <- randomRIO (150000, 300000)
  threadDelay timeToSleep

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  -- , optionsFormatters = [SomeFormatter defaultPrintFormatter]
  , optionsProjectRoot = Just "demos/demo-landing"
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions landingDemo
