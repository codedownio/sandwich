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
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.KindCluster
import Test.Sandwich.Contexts.Kubernetes.MinioOperator
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Waits


spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $ do
  describe "Via kind" $ do
    introduceNixContext nixpkgsReleaseDefault $ do
      introduceKindClusterViaNix defaultKindClusterOptions $ do
        it "prints the cluster info" $ do
          kcc <- getContext kubernetesCluster
          info [i|Got Kubernetes cluster context: #{kcc}|]

        it "prints the loaded images" $ do
          images <- getLoadedImages
          forM_ images $ \image -> info [i|Image: #{image}|]

        withKubernetesNamespace "foo" $
          introduceBinaryViaNixPackage @"kubectl" "kubectl" $
          introduceBinaryViaNixDerivation @"kubectl-minio" kubectlMinioDerivation $
          introduceMinioOperator defaultMinioOperatorOptions $ do
            it "Has a MinIO operator" $ do
              moc <- getContext minioOperator
              info [i|Got MinIO operator: #{moc}|]

            withKubernetesNamespace "foo" $ introduceK8SMinioS3Server (defaultMinioS3ServerOptions "foo") $ do
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
