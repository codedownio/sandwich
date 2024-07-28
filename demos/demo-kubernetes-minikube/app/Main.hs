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
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.MinioOperator
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
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

        it "prints the loaded images" $ do
          images <- getLoadedImages
          forM_ images $ \image -> info [i|Image: #{image}|]

        introduceBinaryViaNixPackage @"kubectl" "kubectl" $
          introduceBinaryViaNixDerivation @"kubectl-minio" kubectlMinioDerivation $
          introduceMinioOperator $ do
            it "Has a MinIO operator" $ do
              moc <- getContext minioOperator
              info [i|Got MinIO operator: #{moc}|]

            withKubernetesNamespace "foo" $ do
              it "creates a service account" $ do
                kubectlBinary <- askFile @"kubectl"

                KubernetesClusterContext {..} <- getContext kubernetesCluster
                baseEnv <- getEnvironment
                let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

                let args = ["create", "serviceaccount", "default", "--namespace", "foo"]

                p <- createProcessWithLogging ((proc kubectlBinary args) { env = Just env, delegate_ctlc = True })
                waitForProcess p >>= (`shouldBe` ExitSuccess)

              introduceK8SMinioS3Server "foo" $ do
                it "has a MinIO S3 server" $ do
                  serv <- getContext testS3Server
                  info [i|Got test S3 server: #{serv}|]

kubectlMinioDerivation :: Text
kubectlMinioDerivation = [i|
{ fetchurl
}:

fetchurl {
  url = "https://github.com/minio/operator/releases/download/v5.0.6/kubectl-minio_5.0.6_linux_amd64";
  hash = "sha256-j3mpgV1HLmFwYRdxfPXT1XzDWeiyQC2Ye8aeZt511bc=";

  downloadToTemp = true;
  executable = true;

  postFetch = ''
    mkdir -p $out/bin
    mv "$downloadedFile" $out/bin/kubectl-minio
  '';
}
|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
