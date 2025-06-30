{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver
import Test.WebDriver.Commands


simple :: TopSpecWithOptions
simple = introduceNixContext (nixpkgsMaster { nixpkgsDerivationAllowUnfree = True }) $
  introduceWebDriverViaNix defaultWdOptions $ do
    it "opens Google and searches" $ withSession1 $ do
      openPage [i|https://www.google.com|]
      search <- findElem (ByCSS [i|*[title="Search"]|])
      click search
      sendKeys "Haskell Sandwich" search
      findElem (ByCSS [i|input[type="submit"]|]) >>= click

      Just dir <- getCurrentFolder
      screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")

      liftIO $ threadDelay 3_000_000

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions simple
