{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.Slack
import Test.Sandwich.Formatters.TerminalUI

simple :: TopSpec
simple = parallel $ do
  it "does the thing 1" sleepThenSucceed
  it "does the thing 2" sleepThenSucceed
  it "does the thing 3" sleepThenFail
  describe "should happen sequentially" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenFail
    it "sequential 3" sleepThenSucceed
  it "does the thing 4" sleepThenFail
  it "does the thing 5" sleepThenSucceed
  it "does the thing 6" sleepThenSucceed


slackFormatter = defaultSlackFormatter {
  slackFormatterSlackConfig = SlackConfig ""
  , slackFormatterTopMessage = Just "Top message"
  , slackFormatterChannel = "test-channel"
  -- , slackFormatterShowFailureReason = False
  -- , slackFormatterShowCallStacks = SlackFormatterNoCallStacks
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

sleepThenSucceed :: ExampleM context ()
sleepThenSucceed = do
  -- liftIO $ threadDelay (2 * 10^1)
  liftIO $ threadDelay (2 * 10^5)
  -- liftIO $ threadDelay (1 * 10^6)
  -- liftIO $ threadDelay (3 * 10^6)

sleepThenFail :: ExampleM context ()
sleepThenFail = do
  -- liftIO $ threadDelay (2 * 10^1)
  liftIO $ threadDelay (2 * 10^5)
  -- liftIO $ threadDelay (1 * 10^6)
  -- liftIO $ threadDelay (3 * 10^6)
  2 `shouldBe` 3
