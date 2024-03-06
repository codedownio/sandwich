{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Sandwich.Contexts.Files
import Sandwich.Contexts.Nix
import Test.Sandwich
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Nix binary" $
  introduceNixContext nixpkgsReleaseDefault $ introduceBinaryViaNixPackage @"hello" "hello" $ do
    it "uses the hello binary" $ do
      helloPath <- askFile @"hello"
      readCreateProcess (proc helloPath []) "" >>= (`shouldBe` "Hello, world!\n")

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
