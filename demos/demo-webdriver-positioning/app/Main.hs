{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands


positioning :: TopSpec
positioning = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "two windows side by side" $ do
    it "opens Google" $ withSession1 $ do
      openPage "http://www.google.com"
      setWindowLeftSide

    it "opens Yahoo" $ withSession2 $ do
      openPage "http://www.yahoo.com"
      setWindowRightSide

    it "pauses" $ do
      liftIO $ threadDelay 5000000

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions positioning
