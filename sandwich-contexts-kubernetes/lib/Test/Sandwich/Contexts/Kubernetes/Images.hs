{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Images (
  introduceImages
  , loadImage
  , loadImage'
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Text as T
import Relude
import Test.Sandwich
import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster.Images as Kind
import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images as Minikube
import Test.Sandwich.Contexts.Kubernetes.Types


-- | Introduce a list of images into a Kubernetes cluster.
introduceImages :: (
  MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context
  ) => [Text] -> SpecFree (LabelValue "kubernetesClusterImages" [Text] :> context) m () -> SpecFree context m ()
introduceImages images = introduceWith "Introduce cluster images" kubernetesClusterImages $ \action ->
  forM images (\x -> loadImage x Nothing) >>= (void . action)

-- | Load an image into a Kubernetes cluster.
loadImage :: (
  MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m, HasKubernetesClusterContext context
  )
  -- | Image name
  => Text
  -- | Optional environment variables to provide
  -> Maybe [(String, String)]
  -- | Returns the transformed image name
  -> m Text
loadImage image env = do
  kcc <- getContext kubernetesCluster
  loadImage' kcc image env

-- | Same as 'loadImage', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
loadImage' :: (
  MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Image name
  -> Text
  -- | Environment variables (currently used only for Kind clusters)
  -> Maybe [(String, String)]
  -- | Callback with transformed image names (see above)
  -> m Text
loadImage' (KubernetesClusterContext {kubernetesClusterType, kubernetesClusterName}) image env = do
  debug [i|Loading container image '#{image}'|]
  timeAction [i|Loading container image '#{image}'|] $ do
    case kubernetesClusterType of
      (KubernetesClusterKind {..}) ->
        Kind.loadImage kindBinary kindClusterName image env
      (KubernetesClusterMinikube {..}) ->
        Minikube.loadImage minikubeBinary kubernetesClusterName minikubeFlags image
