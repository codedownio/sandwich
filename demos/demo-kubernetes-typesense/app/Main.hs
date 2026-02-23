{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.String.Interpolate
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Kubernetes.Typesense
import Test.Sandwich.Contexts.Nix


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $ do
  describe "Via Minikube" $
    introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"kubectl" "kubectl" $
    introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $ do
      it "prints the cluster info" $ do
        kcc <- getContext kubernetesCluster
        info [i|Got Kubernetes cluster context: #{kcc}|]

      withKubernetesNamespace "typesense-test" $ introduceTypesense "typesense-test" defaultTypesenseOptions $ do
        it "Has a Typesense context" $ do
          ts <- getContext typesense
          info [i|Got Typesense context: #{ts}|]

        it "Can describe the Typesense service" $ do
          TypesenseContext {..} <- getContext typesense
          info [i|Typesense service name: #{typesenseServiceName}|]
          info [i|Typesense namespace: #{typesenseNamespace}|]

        it "Pause forever for interaction" $ do
          TypesenseContext {..} <- getContext typesense
          info [i|Typesense is running. You can interact with it now.|]
          info [i|To port-forward: kubectl port-forward -n #{typesenseNamespace} svc/#{typesenseServiceName} 8108:80|]
          info [i|Then access: curl -H "X-TYPESENSE-API-KEY: xyz" http://localhost:8108/health|]
          info [i|Press Ctrl+C to stop.|]
          liftIO $ threadDelay maxBound


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
