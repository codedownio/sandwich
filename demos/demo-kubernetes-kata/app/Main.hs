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
spec = describe "Introducing a Kubernetes cluster via Minikube" $
  introduceNixContext nixpkgsReleaseDefault $
  introduceMinikubeClusterViaNix clusterOptions $
  introduceBinaryViaNixPackage @"kubectl" "kubectl" $
  introduceBinaryViaNixPackage @"helm" "kubernetes-helm" $
  introduceKataContainers (kataOptions undefined) $ do
    it "Has a Kata containers context" $ do
      ctx <- getContext kataContainers
      info [i|Got Kata containers context: #{ctx}|]

    it "pauses" $ do
      threadDelay 9999999999999
  where
    kataOptions = defaultKataContainersOptionsHelmChart {
      kataContainersHelmArgs = [
          "--version", "3.23.0"
          ]
      }

    clusterOptions = defaultMinikubeClusterOptions {
      minikubeClusterDriver = Just "docker"
      , minikubeClusterExtraFlags = ["--container-runtime=containerd"]
      }


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
