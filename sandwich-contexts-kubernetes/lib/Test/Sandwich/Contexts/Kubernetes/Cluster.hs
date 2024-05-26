{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Sandwich.Contexts.Kubernetes.Cluster (
  -- * Kind clusters
  Kind.introduceKindClusterViaNix
  , Kind.introduceKindClusterViaEnvironment
  , Kind.introduceKindCluster'

  -- * Minikube clusters
  , Minikube.introduceMinikubeClusterViaNix
  , Minikube.introduceMinikubeClusterViaEnvironment
  , Minikube.introduceMinikubeCluster'

  -- * Wait for pods/services
  , waitForPodsToExist
  , waitForPodsToBeReady
  , waitForServiceEndpointsToExist

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

  , Minikube.MinikubeClusterOptions(..)
  , Minikube.defaultMinikubeClusterOptions

  -- * Util
  , Util.parseHostnameAndPort
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.URI
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.KubectlLogs
import Test.Sandwich.Contexts.Kubernetes.KubectlPortForward
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Waits

import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster as Kind
import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster.ServiceForwardPortForward as Kind

import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster as Minikube
import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Forwards as Minikube

import qualified Test.Sandwich.Contexts.Kubernetes.Util as Util


withForwardKubernetesService :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context
  ) => Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService namespace serviceName action = do
  kcc <- getContext kubernetesCluster
  withForwardKubernetesService' kcc namespace serviceName action

withForwardKubernetesService' :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m, MonadBaseControl IO m
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService' kcc@(KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterMinikube {..})}) =
  Minikube.withForwardKubernetesService' kcc minikubeProfileName
withForwardKubernetesService' kcc@(KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterKind {})}) =
  Kind.withForwardKubernetesService' kcc
