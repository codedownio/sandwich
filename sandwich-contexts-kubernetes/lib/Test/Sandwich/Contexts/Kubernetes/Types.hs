{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sandwich.Contexts.Kubernetes.Types where

import Kubernetes.OpenAPI.Core as Kubernetes
import Network.HTTP.Client
import Relude
import Test.Sandwich
import qualified Text.Show


instance Show Manager where
  show _ = "<HTTP manager>"

-- * Kubernetes cluster

data KubernetesClusterType =
  KubernetesClusterKind { kindBinary :: FilePath
                        , kindClusterName :: Text
                        , kindClusterDriver :: Text
                        , kindClusterEnvironment :: Maybe [(String, String)] }
  | KubernetesClusterMinikube { minikubeBinary :: FilePath
                              , minikubeProfileName :: Text
                              , minikubeFlags :: [Text] }
  deriving (Show, Eq)

data KubernetesClusterContext = KubernetesClusterContext {
  kubernetesClusterName :: Text
  , kubernetesClusterKubeConfigPath :: FilePath
  , kubernetesClusterNumNodes :: Int
  , kubernetesClusterClientConfig :: (Manager, Kubernetes.KubernetesClientConfig)
  , kubernetesClusterType :: KubernetesClusterType
  } deriving (Show)

kubernetesCluster :: Label "kubernetesCluster" KubernetesClusterContext
kubernetesCluster = Label
type HasKubernetesClusterContext context = HasLabel context "kubernetesCluster" KubernetesClusterContext

-- * Kubernetes cluster images

kubernetesClusterImages :: Label "kubernetesClusterImages" [Text]
kubernetesClusterImages = Label
type HasKubernetesClusterImagesContext context = HasLabel context "kubernetesClusterImages" [Text]

-- * MinIO Operator

data MinioOperatorContext = MinioOperatorContext
  deriving (Show)

minioOperator :: Label "minioOperator" MinioOperatorContext
minioOperator = Label
type HasMinioOperatorContext context = HasLabel context "minioOperator" MinioOperatorContext

-- * SeaweedFS cluster

data SeaweedFSContext = SeaweedFSContext {
  seaweedFsOptions :: SeaweedFSOptions
  } deriving (Show)

data SeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage :: Text
  , seaweedFsBaseName :: Text
  , seaweedFsMasterReplicas :: Int
  , seaweedFsFilerReplicas :: Int
  , seaweedFsVolumeReplicas :: Int
  , seaweedFsVolumeServerDiskCount :: Int
  , seaweedFsVolumeSizeLimitMb :: Int
  , seaweedFsVolumeStorageRequest :: Text
  } deriving (Show)
defaultSeaweedFSOptions :: SeaweedFSOptions
defaultSeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage = "chrislusf/seaweedfs:2.99"
  , seaweedFsBaseName = "seaweed1"
  , seaweedFsMasterReplicas = 3
  , seaweedFsFilerReplicas = 2
  , seaweedFsVolumeReplicas = 1
  , seaweedFsVolumeServerDiskCount = 1
  , seaweedFsVolumeSizeLimitMb = 1024
  , seaweedFsVolumeStorageRequest = "2Gi"
  }

seaweedFs :: Label "seaweedFs" SeaweedFSContext
seaweedFs = Label
type HasSeaweedFSContext context = HasLabel context "seaweedFs" SeaweedFSContext
