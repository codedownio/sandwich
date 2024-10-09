{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

{-|

This is the main module for creating and working with Kubernetes clusters. You can create clusters with either Kind or Minikube, obtaining the relevant binary from either the current PATH or from Nix.

The module also contains functions for waiting for pods and services to exist, running commands with Kubectl, logging, service forwarding, and port forwarding.

-}

module Test.Sandwich.Contexts.Kubernetes.Cluster (
  -- * Kind clusters
  Kind.introduceKindClusterViaNix
  , Kind.introduceKindClusterViaEnvironment
  , Kind.introduceKindCluster'

  , Kind.defaultKindClusterOptions
  , Kind.KindClusterOptions(..)

  -- * Minikube clusters
  , Minikube.introduceMinikubeClusterViaNix
  , Minikube.introduceMinikubeClusterViaEnvironment
  , Minikube.introduceMinikubeCluster'

  , Minikube.defaultMinikubeClusterOptions
  , Minikube.MinikubeClusterOptions(..)

  -- * Wait for pods/services
  , waitForPodsToExist
  , waitForPodsToBeReady
  , waitForServiceEndpointsToExist

  -- * Run commands with kubectl
  , askKubectlArgs
  , askKubectlEnvironment

  -- * Forward services
  , withForwardKubernetesService
  , withForwardKubernetesService'

  -- * Logs
  , module Test.Sandwich.Contexts.Kubernetes.KubectlLogs

  -- * Port forwarding
  , module Test.Sandwich.Contexts.Kubernetes.KubectlPortForward

  -- * Types
  , KubernetesClusterContext (..)
  , kubernetesCluster
  , HasKubernetesClusterContext

  -- * Util
  , Util.parseHostnameAndPort
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Network.URI
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.KubectlLogs
import Test.Sandwich.Contexts.Kubernetes.KubectlPortForward
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Waits

import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster as Kind
import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster.ServiceForwardPortForward as Kind

import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster as Minikube
import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Forwards as Minikube

import qualified Test.Sandwich.Contexts.Kubernetes.Util as Util


-- | Forward a Kubernetes service, so that it can be reached at a local URI.
withForwardKubernetesService :: (
  MonadMask m, KubernetesClusterBasic m context
  )
  -- | Namespace
  => Text
  -- | Service name
  -> Text
  -- | Callback receiving the service 'URL'.
  -> (URI -> m a)
  -> m a
withForwardKubernetesService namespace serviceName action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withForwardKubernetesService' kcc kubectlBinary namespace serviceName action

-- | Same as 'withForwardKubernetesService', but allows you to pass in the 'KubernetesClusterContext' and @kubectl@ binary.
withForwardKubernetesService' :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m
  , HasBaseContextMonad context m
  )
  -- | Kubernetes cluster context
  => KubernetesClusterContext
  -- | Binary path for kubectl
  -> FilePath
  -- | Namespace
  -> Text
  -- | Service name
  -> Text
  -- | Callback receiving the service 'URL'.
  -> (URI -> m a)
  -> m a
withForwardKubernetesService' kcc@(KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterMinikube {..})}) _kubectlBinary =
  Minikube.withForwardKubernetesService' kcc minikubeProfileName
withForwardKubernetesService' kcc@(KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterKind {})}) kubectlBinary =
  Kind.withForwardKubernetesService' kcc kubectlBinary
