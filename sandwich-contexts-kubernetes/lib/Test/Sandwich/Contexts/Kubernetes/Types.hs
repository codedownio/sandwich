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
                        , kindClusterEnvironment :: Maybe [(String, String)]
                        }
  | KubernetesClusterMinikube { minikubeBinary :: FilePath
                              , minikubeProfileName :: Text
                              , minikubeFlags :: [Text]
                              }
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
