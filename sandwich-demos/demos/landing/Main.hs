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
import Test.Sandwich.Formatters.Slack


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
  timeToSleep :: Int <- randomRIO (1500000, 3000000)
  threadDelay timeToSleep

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  , optionsProjectRoot = Just "sandwich-demos"
  , optionsFormatters = [SomeFormatter $ defaultSlackFormatter {
      slackFormatterSlackConfig = SlackConfig ""
      , slackFormatterTopMessage = Just "Arithmetic tests"
      , slackFormatterChannel = "testing"
      , slackFormatterMaxFailures = Just 2
      , slackFormatterMaxFailureReasonLines = Just 1
      , slackFormatterMaxCallStackLines = Just 3
      , slackFormatterVisibilityThreshold = Just 50
      }]
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions landingDemo
