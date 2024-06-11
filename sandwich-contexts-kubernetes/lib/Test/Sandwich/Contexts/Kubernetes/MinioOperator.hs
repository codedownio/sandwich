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
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


-- | Install the MinIO Kubernetes plugin onto a Kubernetes cluster.
-- See the docs [here](https://min.io/docs/minio/kubernetes/upstream/reference/kubectl-minio-plugin.html).
introduceMinioOperator :: (
  MonadUnliftIO m, HasKubernetesClusterContext context, HasFile context "kubectl", HasFile context "kubectl-minio"
  ) => SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator kcc action

-- | Same as 'introduceMinioOperator', but allows you to pass in the "kubectl-minio" binary path.
introduceMinioOperator' :: (
  MonadUnliftIO m, HasKubernetesClusterContext context
  ) => FilePath -> FilePath -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator' kubectlBinary kubectlMinioBinary = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator' kubectlBinary kubectlMinioBinary kcc action

-- | Bracket-style variant of 'introduceMinioOperator'.
withMinioOperator :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadReader context m, HasFile context "kubectl", HasFile context "kubectl-minio"
  ) => KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator kcc action = do
  kubectlBinary <- askFile @"kubectl"
  kubectlMinioBinary <- askFile @"kubectl-minio"
  withMinioOperator' kubectlBinary kubectlMinioBinary kcc action

-- | Same as 'withMinioOperator', but allows you to pass in the "kubectl-minio" binary path.
withMinioOperator' :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => FilePath -> FilePath -> KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator' kubectlBinary kubectlMinioBinary (KubernetesClusterContext {..}) action = do
  baseEnv <- getEnvironment

  let basePathParts = maybe [] splitSearchPath (L.lookup "PATH" baseEnv)

  let newPath = L.intercalate [searchPathSeparator] ((takeDirectory kubectlBinary) : basePathParts)

  let env = L.nubBy (\x y -> fst x == fst y) (("PATH", newPath) : ("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  let runWithKubeConfig exe args = do
        p <- createProcessWithLogging ((proc exe args) { env = Just env, delegate_ctlc = True })
        code <- waitForProcess p
        code `shouldBe` ExitSuccess

  bracket_ (runWithKubeConfig kubectlMinioBinary ["init"])
           -- Can't delete -f yet; see https://github.com/minio/operator/issues/1683
           (return ()) -- (runWithKubeConfig kubectlMinioBinary ["delete"])
           (action MinioOperatorContext)
