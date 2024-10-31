{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.String.Interpolate
import Data.Time
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.FakeSmtpServer
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.KataContainers
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.MinioOperator
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Waits
import UnliftIO.Concurrent
import UnliftIO.Environment
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster via Minikube" $ do
  introduceNixContext nixpkgsReleaseDefault $ do
    introduceMinikubeClusterViaNix clusterOptions $ do
      introduceBinaryViaNixPackage @"kubectl" "kubectl" $ do
        introduceKataContainers defaultKataContainersOptions $ do
          it "Has a Kata containers context" $ do
            ctx <- getContext kataContainers
            info [i|Got Kata containers context: #{ctx}|]

          it "pauses" $ do
            threadDelay 9999999999999
  where
    clusterOptions = defaultMinikubeClusterOptions {
      minikubeClusterDriver = Just "kvm2"
      , minikubeClusterExtraFlags = ["--container-runtime=containerd"]
      }


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
