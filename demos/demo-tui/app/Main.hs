{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Interpolate
import System.Random
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.TerminalUI


simple :: TopSpec
simple = parallel $ do
  describe "Foo" $ do
    it "tests foo #1" pauseRandomAndSucceed
    it "tests foo #2" pauseRandomAndFail

    describe "tests some nested foo" $ do
      it "tests nested #3" pauseRandomAndSucceed

  describe "Bar" $ do
    it "tests bar #1" $ pauseRandomAndSucceed >> warn "That was a weird test"
    it "tests bar #2" pauseRandomAndSucceed

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions simple
