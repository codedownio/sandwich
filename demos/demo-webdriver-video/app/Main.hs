{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Time.Clock
import System.FilePath
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Video
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands
import UnliftIO.Exception


manualVideo :: TopSpec
manualVideo = introduceWebDriver defaultWdOptions $ do
  describe "video recording" $ do
    it "opens Google" $ withSession1 $ do
      openPage "http://www.google.com"

      Just dir <- getCurrentFolder
      let path = dir </> "video" -- No extension needed
      bracket (startBrowserVideoRecording path defaultVideoSettings) endVideoRecording $ \_ -> do
        search <- findElem (ByCSS [i|*[title="Search"]|])
        click search
        sendKeys "Haskell Sandwich" search
        findElem (ByCSS [i|input[type="submit"]|]) >>= click

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions manualVideo
