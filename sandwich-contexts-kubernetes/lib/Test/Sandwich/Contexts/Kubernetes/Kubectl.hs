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
  -- | Return the kubectl binary and env.
  => m (FilePath, [(String, String)])
runWithKubectl = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  runWithKubectl' kcc kubectlBinary

runWithKubectl' :: (
  MonadLoggerIO m
  )
  -- | Kubernetes cluster context
  => KubernetesClusterContext
  -- | Path to kubectl binary
  -> FilePath
  -- | Return the kubectl binary and env.
  -> m (FilePath, [(String, String)])
runWithKubectl' (KubernetesClusterContext {..}) kubectlBinary = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  return (kubectlBinary, env)
