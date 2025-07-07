{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Test.Sandwich
import Test.Sandwich.WebDriver


type SeleniumSpec = forall context. (
  HasBaseContext context
  , HasTestWebDriverContext context
  , HasSomeCommandLineOptions context
  ) => SpecFree context IO ()
