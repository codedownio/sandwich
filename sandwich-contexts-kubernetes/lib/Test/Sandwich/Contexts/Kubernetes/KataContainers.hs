{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|

Install [Kata Containers](https://katacontainers.io) on a Kubernetes cluster.

-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers (
  -- * Introduce Kata Containers
  introduceKataContainers

  -- * Bracket-style versions
  , withKataContainers
  , withKataContainers'

  -- * Types
  , KataContainersOptions(..)
  , SourceCheckout(..)

  , defaultKataContainersOptionsLegacy
  , defaultKataContainersOptionsHelmChart

  , kataContainers
  , KataContainersContext(..)
  , HasKataContainersContext
  ) where

import Control.Monad
import Data.String.Interpolate
import Relude hiding (withFile)
import Safe
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.KataContainers.HelmChart
import Test.Sandwich.Contexts.Kubernetes.KataContainers.Legacy
import Test.Sandwich.Contexts.Kubernetes.KataContainers.Types
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Process


-- | Install Kata Containers on the cluster and introduce a 'KataContainersContext'.
introduceKataContainers :: (
  Typeable context, KubectlBasic context m
  )
  -- | Options
  => KataContainersOptions
  -> SpecFree (ContextWithKataContainers context) m ()
  -> SpecFree context m ()
introduceKataContainers options = introduceWith "introduce KataContainers" kataContainers (void . withKataContainers options)

-- | Bracket-style version of 'introduceKataContainers'.
withKataContainers :: forall context m a. (
  HasCallStack, Typeable context, MonadFail m
  , KubectlBasic context m
  )
  -- | Options
  => KataContainersOptions
  -> (KataContainersContext -> m a)
  -> m a
withKataContainers options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withKataContainers' kcc kubectlBinary options action

-- | Same as 'withKataContainers', but allows you to pass in the 'KubernetesClusterContext' and @kubectl@ binary path.
withKataContainers' :: forall context m a. (
  HasCallStack, Typeable context, MonadFail m
  , KubernetesBasic context m
  )
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -> KataContainersOptions
  -> (KataContainersContext -> m a)
  -> m a
withKataContainers' kcc@(KubernetesClusterContext {..}) kubectlBinary options action = do
  -- Preflight checks
  case kubernetesClusterType of
    KubernetesClusterKind {} -> expectationFailure [i|Can't install Kata Containers on Kind at presenpt.|]
    KubernetesClusterMinikube {..} -> do
      output <- readCreateProcessWithLogging (proc kubernetesClusterTypeMinikubeBinary [
                                                 "--profile", toString kubernetesClusterTypeMinikubeProfileName
                                                 , "ssh", [i|egrep -c 'vmx|svm' /proc/cpuinfo|]
                                                 ]) ""
      case readMay output of
        Just (0 :: Int) -> expectationFailure [i|Preflight check: didn't find "vmx" or "svm" in /proc/cpuinfo. Please make sure virtualization support is enabled.|]
        Just _ -> return ()
        Nothing -> expectationFailure [i|Preflight check: couldn't parse output of minikube ssh "egrep -c 'vmx|svm' /proc/cpuinfo"|]

  case options of
    KataContainersOptionsLegacy {..} ->
      withKataContainersLegacy kcc kubectlBinary options kataContainersSourceCheckout kataContainersKataDeployImage kataContainersPreloadImages kataContainersLabelNode action
    KataContainersOptionsHelmChart {..} ->
      withKataContainersHelmChart' kataContainersHelmBinary kcc options kataContainersHelmChart kataContainersHelmArgs action
