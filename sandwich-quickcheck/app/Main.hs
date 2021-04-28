{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.QuickCheck

quickcheckDemo :: TopSpec
quickcheckDemo = introduceQuickCheck $ do
  prop "List reversal" $ \(xs :: [Int]) -> reverse (reverse xs) == xs

  prop "Failing list reversal" $ \(xs :: [Int]) -> (reverse xs) == xs


testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions quickcheckDemo
