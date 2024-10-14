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

  , createKubernetesNamespace
  , destroyKubernetesNamespace
  ) where

import Control.Monad
import Data.String.Interpolate
import Relude hiding (force)
import System.Exit
import Test.Sandwich
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
withKubernetesNamespace' namespace = bracket_ (createKubernetesNamespace namespace) (destroyKubernetesNamespace False namespace)

-- | Create a Kubernetes namespace.
createKubernetesNamespace :: (
  KubectlBasic context m
  )
  -- | Namespace name
  => Text
  -> m ()
createKubernetesNamespace namespace = do
  let args = ["create", "namespace", toString namespace]
  (kubectl, env) <- askKubectlArgs
  createProcessWithLogging ((proc kubectl args) { env = Just env, delegate_ctlc = True })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

-- | Destroy a Kubernetes namespace.
destroyKubernetesNamespace :: (
  KubectlBasic context m
  )
  -- | Whether to pass @--force@
  => Bool
  -- | Namespace name
  -> Text
  -> m ()
destroyKubernetesNamespace force namespace = do
  let args = ["delete", "namespace", toString namespace]
           <> if force then ["--force"] else []
  (kubectl, env) <- askKubectlArgs
  createProcessWithLogging ((proc kubectl args) { env = Just env, delegate_ctlc = True })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
