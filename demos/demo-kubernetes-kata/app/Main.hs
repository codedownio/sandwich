{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.String.Interpolate
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.KataContainers
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Nix
import UnliftIO.Concurrent


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster via Minikube" $
  introduceNixContext nixpkgsReleaseDefault $
  introduceMinikubeClusterViaNix clusterOptions $
  introduceBinaryViaNixPackage @"kubectl" "kubectl" $
  introduceBinaryViaNixPackage @"helm" "kubernetes-helm" $
  introduceKataContainers kataOptions $ do
    it "Has a Kata containers context" $ do
      ctx <- getContext kataContainers
      info [i|Got Kata containers context: #{ctx}|]

    it "pauses" $ do
      threadDelay 9999999999999
  where
    kataOptions = defaultKataContainersOptions {
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
