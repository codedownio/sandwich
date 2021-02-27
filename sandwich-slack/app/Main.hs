{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.Slack
import Test.Sandwich.Formatters.TerminalUI

simple :: TopSpec
simple = parallel $ do
  it "does the thing 1" sleepThenSucceed
  it "does the thing 2" sleepThenSucceed
  it "does the thing 3" deepSleepThenFail
  describe "should happen sequentially" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenFail
    it "sequential 3" sleepThenSucceed
  it "does the thing 4" deepSleepThenFail
  it "does the thing 5" sleepThenSucceed
  it "does the thing 6" sleepThenSucceed


slackFormatter = defaultSlackFormatter {
  slackFormatterSlackConfig = SlackConfig ""
  , slackFormatterTopMessage = Just "Top message"
  , slackFormatterChannel = "test-channel"

  , slackFormatterMaxFailures = Just 2
  , slackFormatterMaxFailureReasonLines = Just 0
  , slackFormatterMaxFailureReasonOverflowLines = Just 10
  , slackFormatterMaxCallStackLines = Just 1
  , slackFormatterMaxCallStackOverflowLines = Just 10

  , slackFormatterVisibilityThreshold = Just 50
  }

baseFormatter = SomeFormatter defaultTerminalUIFormatter
-- baseFormatter = SomeFormatter defaultPrintFormatter

main :: IO ()
main = runSandwich options simple
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
      , optionsFormatters = [baseFormatter, SomeFormatter slackFormatter]
      }

-- * Util

sleepThenSucceed :: (HasCallStack) => ExampleM context ()
sleepThenSucceed = do
  -- liftIO $ threadDelay (2 * 10^1)
  liftIO $ threadDelay (2 * 10^5)
  -- liftIO $ threadDelay (1 * 10^6)
  -- liftIO $ threadDelay (3 * 10^6)

sleepThenFail :: (HasCallStack) => ExampleM context ()
sleepThenFail = do
  -- liftIO $ threadDelay (2 * 10^1)
  liftIO $ threadDelay (2 * 10^5)
  -- liftIO $ threadDelay (1 * 10^6)
  -- liftIO $ threadDelay (3 * 10^6)
  2 `shouldBe` 3


deepSleepThenFail :: (HasCallStack) => ExampleM context ()
deepSleepThenFail = deepSleepThenFail2

deepSleepThenFail2 :: (HasCallStack) => ExampleM context ()
deepSleepThenFail2 = deepSleepThenFail3

deepSleepThenFail3 :: (HasCallStack) => ExampleM context ()
deepSleepThenFail3 = sleepThenFail
