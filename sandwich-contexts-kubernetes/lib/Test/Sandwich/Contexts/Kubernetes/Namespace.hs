{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Namespace (
  withKubernetesNamespace
  , withKubernetesNamespace'
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.KindCluster
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import UnliftIO.Exception
import UnliftIO.Process


-- | Around-style node to create a Kubernetes namespace, and destroy it at the end.
-- If you're installing something via Helm 3, you may not need this as you can just pass "--create-namespace".
withKubernetesNamespace :: (
  MonadUnliftIO m
  , HasBaseContext context
  , HasLabel context "kubernetesCluster" KubernetesClusterContext
  , HasFile context "kubectl"
  )
  -- | Namespace to create
  => Text
  -> SpecFree context m ()
  -> SpecFree context m ()
withKubernetesNamespace namespace = around [i|Create the '#{namespace}' kubernetes namespace|] (void . bracket_ (create namespace) (destroy namespace))

-- | Same as 'withKubernetesNamespace', but works in an arbitrary monad with reader context.
withKubernetesNamespace' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , MonadReader context m
  , HasBaseContext context
  , HasLabel context "kubernetesCluster" KubernetesClusterContext
  , HasFile context "kubectl"
  )
  -- | Namespace to create
  => Text
  -> m a
  -> m a
withKubernetesNamespace' namespace = bracket_ (create namespace) (destroy namespace)

create :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContext context
  , MonadReader context m
  , HasKubernetesClusterContext context
  , HasFile context "kubectl"
  ) => Text -> m ()
create namespace = do
  let args = ["create", "namespace", toString namespace]
  (kubectl, env) <- runWithKubectl
  createProcessWithLogging ((proc kubectl args) { env = Just env, delegate_ctlc = True })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

destroy :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContext context
  , MonadReader context m
  , HasKubernetesClusterContext context
  , HasFile context "kubectl"
  ) => Text -> m ()
destroy namespace = do
  let args = ["delete", "namespace", toString namespace]
  (kubectl, env) <- runWithKubectl
  createProcessWithLogging ((proc kubectl args) { env = Just env, delegate_ctlc = True })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
