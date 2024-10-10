{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-|
Helper module for working with @kubectl@ processes.
-}

module Test.Sandwich.Contexts.Kubernetes.Kubectl (
  -- * Run commands with kubectl
  askKubectlArgs
  , askKubectlEnvironment
  ) where

import Control.Monad.Logger
import qualified Data.List as L
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Environment


-- | Retrieve the @kubectl@ binary path and the set of environment variables to use when invoking it.
-- Derives these from a 'HasFile' context and the 'KubernetesClusterContext' respectively.
--
-- Useful for running Kubectl commands with 'System.Process.createProcess' etc.
askKubectlArgs :: (
  KubectlBasic m context
  )
  -- | Returns the @kubectl@ binary and environment variables.
  => m (FilePath, [(String, String)])
askKubectlArgs = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  (kubectlBinary, ) <$> askKubectlEnvironment kcc

-- | Same as 'askKubectlArgs', but only returns the environment variables.
askKubectlEnvironment :: (
  MonadLoggerIO m
  )
  -- | Kubernetes cluster context
  => KubernetesClusterContext
  -- | Returns the @kubectl@ binary and environment variables.
  -> m [(String, String)]
askKubectlEnvironment (KubernetesClusterContext {..}) = do
  baseEnv <- getEnvironment
  return $ L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
