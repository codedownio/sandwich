{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Namespace (
  withKubernetesNamespace
  ) where

import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.String.Interpolate
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.KindCluster
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


-- | Around-style node to create a Kubernetes namespace, and destroy it at the end.
-- If you're installing something via Helm 3, you may not need this as you can just pass "--create-namespace".
withKubernetesNamespace :: (
  MonadUnliftIO m, HasLabel context "kubernetesCluster" KubernetesClusterContext, MonadThrow m
  )
  -- | Namespace to create
  => Text
  -> SpecFree context m ()
  -> SpecFree context m ()
withKubernetesNamespace namespace = around [i|Create the '#{namespace}' kubernetes namespace|] (void . bracket_ create destroy)
  where
    create = runWithKubeConfig [i|kubectl create namespace #{namespace}|] >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    destroy = runWithKubeConfig [i|kubectl delete namespace #{namespace}|] >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    runWithKubeConfig cmd = do
      KubernetesClusterContext {..} <- getContext kubernetesCluster
      baseEnv <- getEnvironment
      let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
      createProcessWithLogging ((shell cmd) { env = Just env, delegate_ctlc = True })
