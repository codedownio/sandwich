{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver

simple :: TopSpec
simple = introduceWebDriver wdOptions $ do
  it "does the thing 1" $ withSession1 $ do
    openPage "http://www.google.com"
    setWindowLeftSide
    search <- findElem (ByCSS [i|input[title="Search"]|])
    click search
    sendKeys "asdf" search
    liftIO $ threadDelay 1000000
    sendKeys "fdsa" search
    liftIO $ threadDelay 1000000
    sendKeys "jkl" search
    liftIO $ threadDelay 1000000
    findElem (ByCSS ".does-not-exist")
    expectationFailure "OH NO"
  -- it "does the thing 2" $ withSession2 $ do
  --   openPage "http://www.cnn.com"
  --   setWindowRightSide
  --   liftIO $ threadDelay 1000000

-- concurrent :: TopSpec
-- concurrent = introduceWebDriver wdOptions $ parallel $ do
--   it "does the thing 1" $ withSession1 $ do
--     openPage "http://www.google.com"
--     setWindowLeftSide
--     liftIO $ threadDelay 10000000
--   it "does the thing 2" $ withSession2 $ do
--     openPage "http://www.cnn.com"
--     setWindowRightSide
--     liftIO $ threadDelay 10000000

-- pooled :: TopSpec
-- pooled = do
--   introduce "WebDriver pool" webdriverPool doCreatePool (liftIO . purgePool) $ parallel $ do
--     it "works" (2 `shouldBe` 2)

--     claimWebDriver $ do
--       it "does the thing 1" $ withSession1 $ do
--         openPage "http://www.google.com"
--         setWindowLeftSide
--         liftIO $ threadDelay 1000000

--     claimWebDriver $ do
--       it "does the thing 2" $ withSession1 $ do
--         openPage "http://www.cnn.com"
--         setWindowRightSide
--         liftIO $ threadDelay 1000000

-- doCreatePool = do
--   maybeRunRoot <- getRunRoot
--   let runRoot = fromMaybe "/tmp" maybeRunRoot
--   liftIO $ createPool (allocateWebDriver' runRoot wdOptions) cleanupWebDriver' 1 (5) 4

-- claimWebDriver subspec = introduceWith "Claim webdriver" webdriver wrappedAction subspec
--   where wrappedAction action = do
--           pool <- getContext webdriverPool
--           debug "Trying to claim webdriver"
--           withResource pool (liftIO . action)

-- webdriverPool = Label :: Label "webdriverPool" (Pool WdSession)

wdOptions = (defaultWdOptions "/tmp/tools") {
  -- capabilities = chromeCapabilities
  capabilities = firefoxCapabilities
  -- capabilities = headlessFirefoxCapabilities
  , saveSeleniumMessageHistory = Always
  -- , runMode = Normal
  , runMode = RunHeadless defaultHeadlessConfig
  }

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  -- , optionsFormatters = [SomeFormatter defaultTerminalUIFormatter]
  , optionsFormatters = [SomeFormatter defaultPrintFormatter]
  }

main :: IO ()
main = runSandwich testOptions simple
