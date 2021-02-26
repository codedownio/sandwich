{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Interpolate.IsString
import Data.Time.Clock
import System.Random
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.TerminalUI

simple :: TopSpec
simple = parallel $ do
  describe "Foo" $ do
    it "tests foo #1" $ pauseAndSucceed
    it "tests foo #2" $ pauseAndFail

    describe "tests some nested foo" $ do
      it "tests nested #3" $ pauseAndSucceed

  describe "Bar" $ do
    it "tests bar #1" $ pauseAndSucceed >> warn "That was a weird test"
    it "tests bar #2" $ pauseAndSucceed

pauseAndSucceed = pause >> 2 `shouldBe` 2
pauseAndFail = pause >> 2 `shouldBe` 3
pause = liftIO $ do
  pauseTime <- randomRIO (1000000, 4000000)
  threadDelay pauseTime

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions simple
