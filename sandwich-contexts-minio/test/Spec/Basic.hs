module Spec.Basic where

import Data.String.Interpolate
import Relude
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import UnliftIO.Directory


tests :: TopSpec
tests = describe "Tests" $ do
  it "should work" $ do
    2 `shouldBe` 2
