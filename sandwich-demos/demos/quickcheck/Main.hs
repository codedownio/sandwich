{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Time.Clock
import Test.QuickCheck
import Test.Sandwich


quickCheckDemo :: TopSpec
quickCheckDemo = describe "QuickCheck tests" $ do
  it "Tests a thing" $ do
    result <- liftIO $ quickCheckWithResult stdArgs $ \(xs :: [Int]) -> reverse xs == xs
    info [i|Got result: #{result}|]

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions quickCheckDemo
