{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SeleniumTests.SeleniumTests1 where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.WebDriver
import Types


tests :: SeleniumSpec
tests = describe "Selenium tests 1" $ do
  it "opens Google and searches" $ withSession1 $ do
    openPage [i|https://www.google.com|]
    search <- findElem (ByCSS [i|*[title="Search"]|])
    click search
    sendKeys "Haskell Sandwich" search

    findElem (ByCSS [i|input[type="submit"]|]) >>= click

    liftIO $ threadDelay 5000000
