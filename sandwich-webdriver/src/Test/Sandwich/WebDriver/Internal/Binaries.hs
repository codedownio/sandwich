{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Test.Sandwich.WebDriver.Internal.Binaries (
  obtainSelenium
  , downloadSeleniumIfNecessary
  , SeleniumToUse(..)

  , obtainChromeDriver
  , ChromeDriverToUse(..)
  , downloadChromeDriverIfNecessary

  , obtainGeckoDriver
  , GeckoDriverToUse(..)
  ) where

import Test.Sandwich.WebDriver.Internal.Binaries.Chrome
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Types
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium.Types
