{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Test.Sandwich.WebDriver.Binaries (
  obtainSelenium
  , downloadSeleniumIfNecessary
  , SeleniumToUse(..)

  , obtainChromeDriver
  , ChromeDriverToUse(..)
  , downloadChromeDriverIfNecessary

  , obtainGeckoDriver
  , GeckoDriverToUse(..)
  , GeckoDriverVersion(..)
  ) where

import Test.Sandwich.WebDriver.Internal.Binaries.Chrome
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium
