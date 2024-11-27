{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Control.Monad
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import UnliftIO.Directory
import UnliftIO.Exception

nixDemo :: TopSpec
nixDemo =
  introduceNixContext nixpkgsRelease2405 $
    introduceNixEnvironment ["emacs", "firefox"] $ do
      it "Uses the Nix environment" $ do
        env <- getContext nixEnvironment
        binaries <- listDirectory (env </> "bin")
        info [i|Found binaries in environment: #{binaries}|]


testOptions = defaultOptions { optionsTestArtifactsDirectory = defaultTestArtifactsDirectory }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions nixDemo
