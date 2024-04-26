{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.Kubernetes.MinioOperator (
  MinioOperatorContext(..)
  , HasMinioOperatorContext

  , introduceMinioOperator

  , withMinioOperator
  ) where

import Sandwich.Contexts.Kubernetes.Types
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Relude
import System.Exit
import Test.Sandwich
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


-- * Introduce

introduceMinioOperator :: (
  MonadUnliftIO m, HasKubernetesClusterContext context
  ) => SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator kcc action

-- * Implementation

withMinioOperator :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator (KubernetesClusterContext {..}) action = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
  let runWithKubeConfig cmd = do
        p <- createProcessWithLogging ((shell cmd) { env = Just env, delegate_ctlc = True })
        code <- waitForProcess p
        code `shouldBe` ExitSuccess

  bracket_ (runWithKubeConfig [i|kubectl-minio init|])
           -- Can't delete -f yet; see https://github.com/minio/operator/issues/1683
           (return ()) -- (runWithKubeConfig [i|kubectl-minio delete|])
           (action MinioOperatorContext)
