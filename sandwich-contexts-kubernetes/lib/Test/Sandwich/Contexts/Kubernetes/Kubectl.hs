{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.Contexts.Kubernetes.Kubectl (
  -- * Run commands with kubectl
  runWithKubectl
  , runWithKubectl'
  ) where

import Control.Monad.Logger
import qualified Data.List as L
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Environment


runWithKubectl :: (
  MonadLoggerIO m
  , HasBaseContextMonad context m, HasFile context "kubectl", HasKubernetesClusterContext context
  )
  -- | Callback receiving the kubectl binary and env.
  => (FilePath -> [(String, String)] -> m a)
  -> m a
runWithKubectl cb = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  runWithKubectl' kcc kubectlBinary cb

runWithKubectl' :: (
  MonadLoggerIO m
  , HasBaseContextMonad context m
  )
  -- | Kubernetes cluster context
  => KubernetesClusterContext
  -- | Binary path for kubectl
  -> FilePath
  -- | Callback receiving the kubectl binary and env.
  -> (FilePath -> [(String, String)] -> m a)
  -> m a
runWithKubectl' (KubernetesClusterContext {..}) kubectlBinary cb = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  cb kubectlBinary env
