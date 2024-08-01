{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
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
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.MinioOperator
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Kubernetes.SeaweedFS
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
import UnliftIO.Environment
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $ do
  describe "Via Minikube" $
    introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"kubectl" "kubectl" $
    introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $ do
      it "prints the cluster info" $ do
        kcc <- getContext kubernetesCluster
        info [i|Got Kubernetes cluster context: #{kcc}|]

      withKubernetesNamespace "foo" $ introduceSeaweedFS "foo" defaultSeaweedFSOptions $ do
        it "Has a SeaweedFS context" $ do
          sfs <- getContext seaweedFs
          info [i|Got SeaweedFS context: #{sfs}|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
