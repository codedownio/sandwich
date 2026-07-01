{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sandwich.Contexts.Kubernetes.Types where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Kubernetes.OpenAPI.Core as Kubernetes
import Network.HTTP.Client
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import qualified Text.Show
import UnliftIO.Environment (getEnvironment)


-- | Append extra environment variables over a base environment, with the extras superseding any
-- base entry of the same name (and later duplicates within either list dropped). This is the
-- list-at-a-time analogue of an @addOrReplace@ over an association list.
addOrReplaceEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
addOrReplaceEnv extra base = L.nubBy ((==) `on` fst) (extra <> base)

-- | Build the @env@ for a subprocess by appending the given extra variables over the current ambient
-- environment. Returns 'Nothing' when there are no extras, so callers pass @env = Nothing@ and inherit
-- the environment unchanged (behaviour identical to not touching @env@ at all).
minikubeProcEnv :: MonadIO m => [(String, String)] -> m (Maybe [(String, String)])
minikubeProcEnv [] = pure Nothing
minikubeProcEnv extra = Just . addOrReplaceEnv extra <$> getEnvironment


instance Show Manager where
  show _ = "<HTTP manager>"

-- * Kubernetes cluster

data KubernetesClusterType =
  KubernetesClusterKind { kubernetesClusterTypeKindBinary :: FilePath
                        , kubernetesClusterTypeKindClusterName :: Text
                        , kubernetesClusterTypeKindClusterDriver :: Text
                        , kubernetesClusterTypeKindClusterEnvironment :: Maybe [(String, String)]
                        }
  | KubernetesClusterMinikube { kubernetesClusterTypeMinikubeBinary :: FilePath
                              , kubernetesClusterTypeMinikubeProfileName :: Text
                              , kubernetesClusterTypeMinikubeFlags :: [Text]
                              -- | Extra environment variables to add to every @minikube@ invocation
                              -- for this cluster (image loads, service forwards, @ssh@, delete).
                              -- These are appended over the ambient process environment at call time,
                              -- superseding any ambient variable of the same name (see 'addOrReplaceEnv').
                              -- Use this to scope e.g. @MINIKUBE_HOME@ to a single cluster without
                              -- mutating the global process environment.
                              , kubernetesClusterTypeMinikubeExtraEnvironment :: [(String, String)]
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

-- * Contexts with MonadReader

type KubernetesBasic context m = (
  MonadLoggerIO m
  , MonadUnliftIO m
  , HasBaseContextMonad context m
  )

type KubernetesClusterBasic context m = (
  KubernetesBasic context m
  , HasKubernetesClusterContext context
  )

type KubectlBasic context m = (
  KubernetesClusterBasic context m
  , HasFile context "kubectl"
  )

type NixContextBasic context m = (
  MonadLoggerIO m
  , MonadUnliftIO m
  , HasBaseContextMonad context m
  , HasNixContext context
  )

-- * Context with MonadReader

type KubernetesBasicWithoutReader context m = (
  MonadLoggerIO m
  , MonadUnliftIO m
  , HasBaseContext context
  )

type KubernetesClusterBasicWithoutReader context m = (
  MonadUnliftIO m
  , HasBaseContext context
  , HasKubernetesClusterContext context
  )

type KubectlBasicWithoutReader context m = (
  MonadUnliftIO m
  , HasBaseContext context
  , HasKubernetesClusterContext context
  , HasFile context "kubectl"
  )

-- * Kubernetes cluster images

kubernetesClusterImages :: Label "kubernetesClusterImages" [Text]
kubernetesClusterImages = Label
type HasKubernetesClusterImagesContext context = HasLabel context "kubernetesClusterImages" [Text]

data ImagePullPolicy = Always | IfNotPresent | Never
  deriving (Show, Eq)

data ImageLoadSpec =
  -- | A @.tar@ or @.tar.gz@ file
  ImageLoadSpecTarball FilePath
  -- | An image pulled via Docker
  | ImageLoadSpecDocker { imageName :: Text
                        , pullPolicy :: ImagePullPolicy }
  -- | An image pulled via Podman
  | ImageLoadSpecPodman { imageName :: Text
                        , pullPolicy :: ImagePullPolicy }
  deriving (Show, Eq)
