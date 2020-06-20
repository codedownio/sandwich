module Main where

import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Types.Options
import Test.Sandwich.WebDriver
import Test.WebDriver

wdOptions = defaultWdOptions "/tmp/tools"

simple :: TopSpec
simple = introduceWebdriver wdOptions $ do
  it "does the thing 1" $ do
    wdSession <- getContext webdriver
    withBrowser1 $ do
      openPage "www.google.com"
    return ()
  it "does the thing 2" $ do
    return ()

options = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwich options defaultTerminalUIFormatter simple
