{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinioOperator (
  introduceMinioOperator
  , introduceMinioOperator'

  -- * Bracket-style variants
  , withMinioOperator
  , withMinioOperator'

  -- * Types
  , minioOperator
  , MinioOperatorContext(..)
  , HasMinioOperatorContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


-- | Install the MinIO Kubernetes plugin onto a Kubernetes cluster.
-- See the docs [here](https://min.io/docs/minio/kubernetes/upstream/reference/kubectl-minio-plugin.html).
introduceMinioOperator :: (
  MonadUnliftIO m, HasKubernetesClusterContext context, HasFile context "kubectl-minio"
  ) => SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator kcc action

-- | Same as 'introduceMinioOperator', but allows you to pass in the "kubectl-minio" binary path.
introduceMinioOperator' :: (
  MonadUnliftIO m, HasKubernetesClusterContext context
  ) => FilePath -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator' kubectlMinioBinary = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator' kubectlMinioBinary kcc action

-- | Bracket-style variant of 'introduceMinioOperator'.
withMinioOperator :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadReader context m, HasFile context "kubectl-minio"
  ) => KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator kcc action = do
  kubectlMinioBinary <- askFile @"kubectl-minio"
  withMinioOperator' kubectlMinioBinary kcc action

-- | Same as 'withMinioOperator', but allows you to pass in the "kubectl-minio" binary path.
withMinioOperator' :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => FilePath -> KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator' kubectlMinioBinary (KubernetesClusterContext {..}) action = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
  let runWithKubeConfig exe args = do
        p <- createProcessWithLogging ((proc exe args) { env = Just env, delegate_ctlc = True })
        code <- waitForProcess p
        code `shouldBe` ExitSuccess

  bracket_ (runWithKubeConfig kubectlMinioBinary ["init"])
           -- Can't delete -f yet; see https://github.com/minio/operator/issues/1683
           (return ()) -- (runWithKubeConfig kubectlMinioBinary ["delete"])
           (action MinioOperatorContext)
