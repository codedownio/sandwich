{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands


positioning :: TopSpecWithOptions
positioning = introduceNixContext (nixpkgsMaster { nixpkgsDerivationAllowUnfree = True }) $
  introduceWebDriverViaNix defaultWdOptions $ do
    describe "two windows side by side" $ do
      it "opens Google" $ withSession1 $ do
        openPage "http://www.google.com"
        setWindowLeftSide

      it "opens xkcd" $ withSession2 $ do
        openPage "http://www.xkcd.com"
        setWindowRightSide

      it "pauses" $ do
        liftIO $ threadDelay 5000000

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions positioning
