{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.WebDriver.Commands


simple :: TopSpec
simple = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  it "opens Google and searches" $ withSession1 $ do
    openPage [i|https://www.google.com|]
    search <- findElem (ByCSS [i|input[title="Search"]|])
    click search
    sendKeys "Haskell Sandwich" search

    findElem (ByCSS [i|input[type="submit"]|]) >>= click

    liftIO $ threadDelay 999999999

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions simple
