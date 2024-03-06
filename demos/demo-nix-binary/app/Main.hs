{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Sandwich.Contexts.Files
import Sandwich.Contexts.Nix
import Test.Sandwich
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Nix binary" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"hello" "hello" $ do
      it "uses the hello binary" $ do
        useHello


useHello :: (MonadIO m, MonadReader context m, HasFile context "hello") => m ()
useHello = do
  helloPath <- askFile @"hello"
  readCreateProcess (proc helloPath []) "" >>= (`shouldBe` "Hello, world!\n")

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
