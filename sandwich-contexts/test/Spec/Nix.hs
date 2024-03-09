module Spec.Nix where

import Test.Sandwich.Contexts.Nix
import Data.String.Interpolate
import Relude
import System.FilePath
import Test.Sandwich
import UnliftIO.Directory


tests :: TopSpec
tests = describe "Nix" $ do
  introduceNixContext nixpkgsReleaseDefault $ do
    it "can build a Nix environment with some binaries" $ do
      envPath <- buildNixSymlinkJoin ["hello", "htop"]
      info [i|Got envPath: #{envPath}|]

      doesFileExist (envPath </> "bin" </> "hello") >>= (`shouldBe` True)
      doesFileExist (envPath </> "bin" </> "htop") >>= (`shouldBe` True)
