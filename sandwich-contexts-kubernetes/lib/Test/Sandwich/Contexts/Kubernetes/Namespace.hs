{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|
Helper module for working with Kubernetes namespaces.
-}

module Test.Sandwich.Contexts.Kubernetes.Namespace (
  withKubernetesNamespace
  , withKubernetesNamespace'
  , withKubernetesNamespace''
  , withKubernetesNamespace'''

  -- * Create a namespace
  , createKubernetesNamespace
  , createKubernetesNamespace'
  , createKubernetesNamespace''

  -- * Destroy a namespace
  , destroyKubernetesNamespace
  , destroyKubernetesNamespace'
  , destroyKubernetesNamespace''
  ) where

import Control.Monad
import Data.String.Interpolate
import Relude hiding (force)
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Exception
import UnliftIO.Process


-- | Around-style node to create a Kubernetes namespace, and destroy it at the end.
--
-- If you're installing something via Helm 3, you may not need this as you can just pass @--create-namespace@.
withKubernetesNamespace :: (
  KubectlBasicWithoutReader context m
  )
  -- | Namespace to create
  => Text
  -> SpecFree context m ()
  -> SpecFree context m ()
withKubernetesNamespace namespace = around [i|Create the '#{namespace}' kubernetes namespace|]
  (void . bracket_ (createKubernetesNamespace namespace) (destroyKubernetesNamespace False namespace))

-- | Same as 'withKubernetesNamespace', but works in an arbitrary monad with reader context.
withKubernetesNamespace' :: (
  KubectlBasic context m
  )
  -- | Namespace to create
  => Text
  -> m a
  -> m a
withKubernetesNamespace' namespace =
  bracket_ (createKubernetesNamespace namespace) (destroyKubernetesNamespace False namespace)

-- | Same as 'withKubernetesNamespace'', but allows you to pass in the path to the @kubectl@ binary.
withKubernetesNamespace'' :: (
  KubernetesClusterBasic context m
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -- | Namespace to create
  -> Text
  -> m a
  -> m a
withKubernetesNamespace'' kubectl namespace =
  bracket_ (createKubernetesNamespace' kubectl namespace) (destroyKubernetesNamespace' kubectl False namespace)

-- | Same as 'withKubernetesNamespace''', but allows you to pass in the cluster context.
withKubernetesNamespace''' :: (
  KubernetesClusterBasic context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -- | Namespace to create
  -> Text
  -> m a
  -> m a
withKubernetesNamespace''' kcc kubectl namespace =
  bracket_ (createKubernetesNamespace'' kcc kubectl namespace) (destroyKubernetesNamespace'' kcc kubectl False namespace)

-- | Create a Kubernetes namespace.
createKubernetesNamespace :: (
  KubectlBasic context m
  )
  -- | Namespace name
  => Text
  -> m ()
createKubernetesNamespace namespace =
  askFile @"kubectl" >>= flip createKubernetesNamespace' namespace

-- | Create a Kubernetes namespace.
createKubernetesNamespace' :: (
  KubernetesClusterBasic context m
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -- | Namespace name
  -> Text
  -> m ()
createKubernetesNamespace' kubectl namespace =
  getContext kubernetesCluster >>= (\kcc -> createKubernetesNamespace'' kcc kubectl namespace)

-- | Destroy a Kubernetes namespace.
destroyKubernetesNamespace :: (
  KubectlBasic context m
  )
  -- | Whether to pass @--force@
  => Bool
  -- | Namespace name
  -> Text
  -> m ()
destroyKubernetesNamespace force namespace =
  askFile @"kubectl" >>= (\x -> destroyKubernetesNamespace' x force namespace)

-- | Destroy a Kubernetes namespace.
destroyKubernetesNamespace' :: (
  KubernetesClusterBasic context m
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -- | Whether to pass @--force@
  -> Bool
  -- | Namespace name
  -> Text
  -> m ()
destroyKubernetesNamespace' kubectl force namespace = do
  getContext kubernetesCluster >>= (\kcc -> destroyKubernetesNamespace'' kcc kubectl force namespace)

-- | Create a Kubernetes namespace.
createKubernetesNamespace'' :: (
  KubernetesClusterBasic context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -- | Namespace name
  -> Text
  -> m ()
createKubernetesNamespace'' kcc kubectl namespace = do
  let args = ["create", "namespace", toString namespace]
  env <- getKubectlEnvironment kcc
  createProcessWithLogging ((proc kubectl args) { env = Just env, delegate_ctlc = True })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

-- | Destroy a Kubernetes namespace.
destroyKubernetesNamespace'' :: (
  KubernetesClusterBasic context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -- | Whether to pass @--force@
  -> Bool
  -- | Namespace name
  -> Text
  -> m ()
destroyKubernetesNamespace'' kcc kubectl force namespace = do
  let args = ["delete", "namespace", toString namespace]
           <> if force then ["--force"] else []
  env <- getKubectlEnvironment kcc
  createProcessWithLogging ((proc kubectl args) { env = Just env, delegate_ctlc = True })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
