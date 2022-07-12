{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Time.Clock
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Sandwich
import Test.Sandwich.Hedgehog


quickCheckDemo :: TopSpec
quickCheckDemo = describe "QuickCheck tests" $ introduceHedgehog $ do
  prop "List reversal" $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

  prop "Failing list reversal" $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse xs === xs

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions quickCheckDemo
