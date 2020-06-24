{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.Pool
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

concurrent :: TopSpec
concurrent = introduceWebdriver wdOptions $ parallel $ do
  it "does the thing 1" $ withBrowser1 $ do
    openPage "http://www.google.com"
    setWindowLeftSide
    liftIO $ threadDelay 10000000
  it "does the thing 2" $ withBrowser2 $ do
    openPage "http://www.cnn.com"
    setWindowRightSide
    liftIO $ threadDelay 10000000

pooled :: TopSpec
pooled = do
  introduce "WebDriver pool" webdriverPool doCreatePool (liftIO . purgePool) $ parallel $ do
    it "works" (2 `shouldBe` 2)

    claimWebDriver $ do
      it "does the thing 1" $ withBrowser1 $ do
        openPage "http://www.google.com"
        setWindowLeftSide
        liftIO $ threadDelay 1000000

    claimWebDriver $ do
      it "does the thing 2" $ withBrowser1 $ do
        openPage "http://www.cnn.com"
        setWindowRightSide
        liftIO $ threadDelay 1000000

doCreatePool = do
  maybeRunRoot <- getRunRoot
  let runRoot = fromMaybe "/tmp" maybeRunRoot
  liftIO $ createPool (allocateWebDriver' runRoot wdOptions) cleanupWebDriver' 1 (5) 4

claimWebDriver subspec = introduceWith "Claim webdriver" webdriver wrappedAction subspec
  where wrappedAction action = do
          debug "Trying to claim webdriver"
          pool <- getContext webdriverPool
          withResource pool (liftIO . action)

wdOptions = (defaultWdOptions "/tmp/tools") {
  capabilities = chromeCapabilities
  , saveSeleniumMessageHistory = Always
  , runMode = Normal
  }

webdriverPool = Label :: Label "webdriverPool" (Pool WdSession)

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwich testOptions defaultTerminalUIFormatter pooled
