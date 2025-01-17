{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Nix derivation" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceFileViaNixDerivation @"firefox" firefoxDerivation $ do
      it "uses the hello binary" $ do
        useFirefox

useFirefox :: (MonadIO m, MonadReader context m, MonadLogger m, HasFile context "firefox") => m ()
useFirefox = do
  firefox <- askFile @"firefox"
  output <- readCreateProcess (proc firefox ["--version"]) ""
  info [i|Firefox version output: #{output}|]

firefoxDerivation = [i|
{firefox}:

firefox
|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
