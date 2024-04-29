{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Namespace (
  withKubernetesNamespace
  ) where

import Test.Sandwich.Contexts.Kubernetes.KindCluster
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.List as L
import Data.String.Interpolate
import Relude
import System.Exit
import Test.Sandwich
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


withKubernetesNamespace :: (
  MonadUnliftIO m, HasLabel context "kubernetesCluster" KubernetesClusterContext, MonadBaseControl IO m, MonadThrow m
  ) => Text -> SpecFree context m () -> SpecFree context m ()
withKubernetesNamespace namespace = around [i|Create the '#{namespace}' kubernetes namespace|] (void . bracket_ create destroy)
  where
    create = runWithKubeConfig [i|kubectl create namespace #{namespace}|] >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    destroy = runWithKubeConfig [i|kubectl delete namespace #{namespace}|] >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    runWithKubeConfig cmd = do
      KubernetesClusterContext {..} <- getContext kubernetesCluster
      baseEnv <- getEnvironment
      let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
      createProcessWithLogging ((shell cmd) { env = Just env, delegate_ctlc = True })
