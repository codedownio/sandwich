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
import Test.Sandwich.Contexts.Waits
import UnliftIO.Environment
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster via Minikube" $ do
  introduceNixContext nixpkgsReleaseDefault $ do
    introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $ do
      it "prints the cluster info" $ do
        kcc <- getContext kubernetesCluster
        info [i|export KUBECONFIG='#{kubernetesClusterKubeConfigPath kcc}'|]
        info [i|Got Kubernetes cluster context: #{kcc}|]

      it "prints the loaded images" $ do
        images <- getLoadedImages
        forM_ images $ \image -> info [i|Image: #{image}|]

      introduceBinaryViaNixPackage @"kubectl" "kubectl" $ do
        introduceKataContainers defaultKataContainersOptions $ do
          it "Has a Kata containers context" $ do
            ctx <- getContext kataContainers
            info [i|Got Kata containers context: #{ctx}|]

          -- it "pauses" $ do
          --   threadDelay 9999999999999

        introduceMinioOperator defaultMinioOperatorOptions $ do
          it "Has a MinIO operator" $ do
            moc <- getContext minioOperator
            info [i|Got MinIO operator: #{moc}|]

          withKubernetesNamespace "foo" $ do
            introduceK8SMinioS3Server (defaultMinioS3ServerOptions "foo") $ do
              it "has a MinIO S3 server" $ do
                serv <- getContext testS3Server
                info [i|Got test S3 server: #{serv}|]

              -- it "pauses" $ do
              --   threadDelay 9999999999999


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
