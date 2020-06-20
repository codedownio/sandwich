module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Types.Options
import Test.Sandwich.WebDriver
import Test.WebDriver

wdOptions = (defaultWdOptions "/tmp/tools") {
        capabilities = chromeCapabilities
        , saveSeleniumMessageHistory = Always
        , runMode = Normal
        }

simple :: TopSpec
simple = introduceWebdriver wdOptions $ do
  it "does the thing 1" $ do
    wdSession <- getContext webdriver
    withBrowser1 $ do
      openPage "http://www.google.com"
      liftIO $ threadDelay 10000000
    return ()
  it "does the thing 2" $ do
    return ()

options = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwich options defaultTerminalUIFormatter simple
