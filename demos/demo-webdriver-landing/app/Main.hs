{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands


spec :: TopSpecWithOptions
spec = introduceNixContext (nixpkgsReleaseDefault { nixpkgsDerivationAllowUnfree = True }) $ introduceWebDriverViaNix defaultWdOptions $ do
  before "Position window" (withSession1 setWindowRightSide) $ do
    it "opens Google" $ withSession1 $ do
      openPage [i|https://www.google.com|]
      liftIO $ threadDelay 2000000

    it "opens Yahoo" $ withSession1 $ do
      openPage [i|https://www.yahoo.com|]
      liftIO $ threadDelay 2000000

    it "opens Bing" $ withSession1 $ do
      openPage [i|https://www.bing.com|]
      liftIO $ threadDelay 2000000

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions spec
