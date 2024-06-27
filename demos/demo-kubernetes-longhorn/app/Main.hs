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
import Test.Sandwich.Contexts.Kubernetes.Longhorn
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.MinioOperator
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
import UnliftIO.Concurrent
import UnliftIO.Environment
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $ do
  describe "Via Minikube" $ do
    introduceNixContext nixpkgsReleaseDefault $ do
      introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $ do
        it "prints the cluster info" $ do
          kcc <- getContext kubernetesCluster
          info [i|Got Kubernetes cluster context: #{kcc}|]

        introduceLonghorn defaultLonghornOptions $ do
          it "Has a Longhorn context" $ do
            x <- getContext longhorn
            info [i|Got Longhorn context: #{x}|]

          it "Pauses for 5 minutes for examination" $ do
            kcc <- getContext kubernetesCluster
            debug [i|export KUBECONFIG='#{kubernetesClusterKubeConfigPath kcc}'|]
            threadDelay 300_000_000


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
