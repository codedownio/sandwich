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
  , introduceKataContainers'

  -- * Bracket-style versions
  , withKataContainers
  , withKataContainers'

  -- * Types
  , KataContainersOptions(..)

  , defaultKataContainersOptions

  , kataContainers
  , KataContainersContext(..)
  , HasKataContainersContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Relude hiding (withFile)
import Safe
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import qualified Test.Sandwich.Contexts.Kubernetes.KataContainers.HelmChart as HC
import Test.Sandwich.Contexts.Kubernetes.KataContainers.Types
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Process


-- | Install Kata Containers on the cluster and introduce a 'KataContainersContext'.
introduceKataContainers :: (
  MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context, HasFile context "helm"
  )
  -- | Options
  => KataContainersOptions
  -> SpecFree (ContextWithKataContainers context) m ()
  -> SpecFree context m ()
introduceKataContainers options = introduceWith "introduce KataContainers" kataContainers (void . withKataContainers options)

-- | Same as 'introduceKataContainers', but allows you to pass in the 'KubernetesClusterContext' and binary paths.
introduceKataContainers' :: (
  MonadUnliftIO m, HasBaseContext context
  )
  => KubernetesClusterContext
  -- | Path to @helm@ binary
  -> FilePath
  -- | Options
  -> KataContainersOptions
  -> SpecFree (ContextWithKataContainers context) m ()
  -> SpecFree context m ()
introduceKataContainers' kcc helmBinary options = introduceWith "introduce KataContainers" kataContainers (void . withKataContainers' kcc helmBinary options)

-- | Bracket-style version of 'introduceKataContainers'.
withKataContainers :: forall context m a. (
  HasCallStack, MonadFail m
  , KubernetesClusterBasic context m, HasFile context "helm"
  )
  -- | Options
  => KataContainersOptions
  -> (KataContainersContext -> m a)
  -> m a
withKataContainers options action = do
  kcc <- getContext kubernetesCluster
  helmBinary <- askFile @"helm"
  withKataContainers' kcc helmBinary options action

-- | Same as 'withKataContainers', but allows you to pass in the 'KubernetesClusterContext' and binary paths.
withKataContainers' :: forall context m a. (
  HasCallStack, MonadFail m
  , KubernetesBasic context m
  )
  => KubernetesClusterContext
  -- | Path to @helm@ binary
  -> FilePath
  -> KataContainersOptions
  -> (KataContainersContext -> m a)
  -> m a
withKataContainers' kcc@(KubernetesClusterContext {..}) helmBinary options action = do
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

  HC.withKataContainers' helmBinary kcc options action
