{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Time.Clock
import Test.QuickCheck
import Test.Sandwich
import Test.Sandwich.QuickCheck


quickCheckDemo :: TopSpec
quickCheckDemo = describe "QuickCheck tests" $ introduceQuickCheck $ do
  prop "List reversal" $ \(xs :: [Int]) -> reverse (reverse xs) == xs
  prop "Failing list reversal" $ \(xs :: [Int]) -> (reverse xs) == xs

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions quickCheckDemo
