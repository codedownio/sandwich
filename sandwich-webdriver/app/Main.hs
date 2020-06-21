module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Types.Options
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver

simple :: TopSpec
simple = introduceWebdriver wdOptions $ do
  it "does the thing 1" $ withBrowser1 $ do
    openPage "http://www.google.com"
    setWindowLeftSide
    liftIO $ threadDelay 10000000
  it "does the thing 2" $ withBrowser2 $ do
    openPage "http://www.cnn.com"
    setWindowRightSide
    liftIO $ threadDelay 10000000

wdOptions = (defaultWdOptions "/tmp/tools") {
        capabilities = chromeCapabilities
        , saveSeleniumMessageHistory = Always
        , runMode = Normal
        }

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwich testOptions defaultTerminalUIFormatter simple
