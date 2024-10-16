{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

{-|
Obtain various binaries you might need for WebDriver testing.
-}

module Test.Sandwich.WebDriver.Binaries (
  -- * Selenium
  obtainSelenium
  , SeleniumToUse(..)

  -- * Chrome
  , obtainChrome
  , ChromeToUse(..)
  , ChromeVersion(..)

  -- * Chrome driver
  , obtainChromeDriver
  , ChromeDriverToUse(..)
  , ChromeDriverVersion(..)

  -- * Firefox
  , obtainFirefox
  , FirefoxToUse(..)

  -- * Geckodriver
  , obtainGeckoDriver
  , GeckoDriverToUse(..)
  , GeckoDriverVersion(..)

  -- * Ffmpeg
  , obtainFfmpeg
  , FfmpegToUse(..)

  -- * Xvfb
  , obtainXvfb
  , XvfbDependenciesSpec(..)
  , XvfbToUse(..)
  , FluxboxToUse(..)
  ) where

import Test.Sandwich.WebDriver.Internal.Binaries.Chrome
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb
