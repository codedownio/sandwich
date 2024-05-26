{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Data.Time
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.FakeSmtpServer
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.KindCluster
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $ do
  describe "Via kind" $ do
    introduceNixContext nixpkgsReleaseDefault $ do
      introduceKindClusterViaNix defaultKindClusterOptions $ do
        it "prints the Kind cluster info" $ do
          kcc <- getContext kubernetesCluster
          info [i|Got Kubernetes cluster context: #{kcc}|]

          liftIO $ threadDelay 60_000_000

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
