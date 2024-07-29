{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Images (
  getLoadedImages

  , loadImage
  , loadImage'

  , introduceImages
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import Relude
import Test.Sandwich
import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster.Images as Kind
import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images as Minikube
import Test.Sandwich.Contexts.Kubernetes.Types


-- | Get the images loaded onto the cluster.
getLoadedImages :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m, HasKubernetesClusterContext context
  )
  -- | List of image names
  => m (Set Text)
getLoadedImages = getContext kubernetesCluster >>= getLoadedImages'

-- | Same as 'getLoadedImages', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
getLoadedImages' :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | List of image names
  -> m (Set Text)
getLoadedImages' kcc@(KubernetesClusterContext {kubernetesClusterType, kubernetesClusterName}) = do
  timeAction [i|Getting loaded images|] $ do
    case kubernetesClusterType of
      (KubernetesClusterKind {..}) ->
        Kind.getLoadedImages kcc kindClusterDriver kindBinary Nothing
        -- Kind.loadImage kindBinary kindClusterName image env
      (KubernetesClusterMinikube {..}) ->
        Minikube.getLoadedImages minikubeBinary kubernetesClusterName minikubeFlags

-- | Load an image into a Kubernetes cluster. The image you pass may be an absolute path to a .tar or .tar.gz
-- image archive, *or* the name of an image in your local Docker daemon. It will load the image onto the cluster,
-- and return the modified image name (i.e. the name by which the cluster knows the image).
loadImage :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context
  )
  -- | Image name
  => Text
  -- | Optional environment variables to provide
  -> Maybe [(String, String)]
  -- | The transformed image name
  -> m Text
loadImage image env = do
  kcc <- getContext kubernetesCluster
  loadImage' kcc image env

-- | Same as 'loadImage', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
loadImage' :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m, HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Image name
  -> Text
  -- | Environment variables (currently used only for Kind clusters)
  -> Maybe [(String, String)]
  -- | The transformed image name
  -> m Text
loadImage' kcc@(KubernetesClusterContext {kubernetesClusterType, kubernetesClusterName}) image env = do
  debug [i|Loading container image '#{image}'|]
  timeAction [i|Loading container image '#{image}'|] $ do
    case kubernetesClusterType of
      (KubernetesClusterKind {..}) ->
        Kind.loadImage kindBinary kindClusterName image env
      (KubernetesClusterMinikube {..}) -> do
        image' <- Minikube.loadImage minikubeBinary kubernetesClusterName minikubeFlags image

        -- Because of the possible silent failure in "minikube image load", confirm that this
        -- image made it onto the cluster.
        loadedImages <- Set.toList <$> getLoadedImages' kcc
        loadedImages `shouldContain` [image']

        return image'

-- | Helper to introduce a list of images into a Kubernetes cluster.
-- Stores the list of transformed image names under the "kubernetesClusterImages" label.
introduceImages :: (
  HasCallStack, MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context
  ) => [Text] -> SpecFree (LabelValue "kubernetesClusterImages" [Text] :> context) m () -> SpecFree context m ()
introduceImages images = introduceWith "Introduce cluster images" kubernetesClusterImages $ \action ->
  forM images (\x -> loadImage x Nothing) >>= (void . action)
