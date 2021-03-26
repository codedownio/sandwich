{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.QuickCheck

simple :: CoreSpec
simple = do
  it "does the thing 1" $ do
    2 `shouldBe` 2

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwich testOptions simple
