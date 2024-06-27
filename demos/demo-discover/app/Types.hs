{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Types


type SeleniumSpec = forall context. (
  HasBaseContext context
  , HasWebDriverContext context
  ) => SpecFree context IO ()
