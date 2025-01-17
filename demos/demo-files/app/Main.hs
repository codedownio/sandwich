{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Common
import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process


filesDemo :: TopSpec
filesDemo =
  introduceBinaryViaEnvironment @"grep" $ do
    testsWithGrep

testsWithGrep :: (HasFile context "grep") => SpecFree context IO ()
testsWithGrep = do
  it "Uses grep binary" $ do
    grep <- askFile @"grep"
    output <- readCreateProcess (proc grep ["--version"]) ""
    info [i|grep --version output: #{output}|]

testOptions = defaultOptions { optionsTestArtifactsDirectory = defaultTestArtifactsDirectory }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions filesDemo
