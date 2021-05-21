{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands


positioning :: TopSpec
positioning = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "two windows side by side" $ do
    it "opens Google" $ withSession1 $ do
      setWindowLeftSide
      openPage "http://www.google.com"

    it "opens Google" $ withSession2 $ do
      setWindowRightSide
      openPage "http://www.yahoo.com"

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions positioning
