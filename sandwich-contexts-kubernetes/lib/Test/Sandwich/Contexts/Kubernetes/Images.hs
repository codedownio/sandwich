{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Images (
  getLoadedImages
  , getLoadedImages'

  , clusterContainsImage
  , clusterContainsImage'

  , loadImageIfNecessary
  , loadImageIfNecessary'

  , loadImage
  , loadImage'

  , withImageLoadRetry
  , withImageLoadRetry'

  , introduceImages

  , findAllImages
  , findAllImages'

  , ImageLoadSpec(..)
  , ImagePullPolicy(..)
  ) where

import Control.Monad.Catch (Handler(..), MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Retry
import Data.String.Interpolate
import Data.Text as T
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.FindImages
import qualified Test.Sandwich.Contexts.Kubernetes.KindCluster.Images as Kind
import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images as Minikube
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Images


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
        -- Note: don't pass minikubeFlags here. These are pretty much intended for "minikube start" only.
        -- TODO: clarify the documentation and possibly add an extra field where extra options can be passed
        -- to "minikube image" commands.
        Minikube.getLoadedImages minikubeBinary kubernetesClusterName []

-- | Test if a cluster has a given image loaded.
clusterContainsImage :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m, HasKubernetesClusterContext context
  )
  -- | Image
  => Text
  -> m Bool
clusterContainsImage image = do
  kcc <- getContext kubernetesCluster
  clusterContainsImage' kcc image

-- | Same as 'clusterContainsImage', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
clusterContainsImage' :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Image
  -> Text
  -> m Bool
clusterContainsImage' kcc@(KubernetesClusterContext {kubernetesClusterType, kubernetesClusterName}) image = do
  case kubernetesClusterType of
    KubernetesClusterKind {..} ->
      Kind.clusterContainsImage kcc kindClusterDriver kindBinary kindClusterEnvironment image
    KubernetesClusterMinikube {..} ->
      Minikube.clusterContainsImage minikubeBinary kubernetesClusterName [] image

-- | Same as 'loadImage', but first checks if the given image is already present on the cluster.
loadImageIfNecessary :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context
  )
  -- | Image load spec
  => ImageLoadSpec
  -> m ()
loadImageIfNecessary image = do
  kcc <- getContext kubernetesCluster
  loadImageIfNecessary' kcc image

-- | Same as 'loadImage', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
loadImageIfNecessary' :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m, HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Image load spec
  -> ImageLoadSpec
  -- | The transformed image name
  -> m ()
loadImageIfNecessary' kcc imageLoadSpec = do
  unlessM (imageLoadSpecToImageName imageLoadSpec >>= clusterContainsImage' kcc) $
    void $ loadImage' kcc imageLoadSpec

-- | Load an image into a Kubernetes cluster. The image you pass may be an absolute path to a @.tar@ or @.tar.gz@
-- image archive, *or* the name of an image in your local Docker daemon. It will load the image onto the cluster,
-- and return the modified image name (i.e. the name by which the cluster knows the image).
loadImage :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context
  )
  -- | Image load spec
  => ImageLoadSpec
  -- | The loaded image name
  -> m Text
loadImage image = do
  kcc <- getContext kubernetesCluster
  loadImage' kcc image

-- | Same as 'loadImage', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
loadImage' :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m, HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Image load spec
  -> ImageLoadSpec
  -- | The loaded image name
  -> m Text
loadImage' (KubernetesClusterContext {kubernetesClusterType, kubernetesClusterName}) imageLoadSpec = do
  debug [i|Loading container image '#{imageLoadSpec}'|]
  timeAction [i|Loading container image '#{imageLoadSpec}'|] $ do
    case kubernetesClusterType of
      (KubernetesClusterKind {..}) ->
        Kind.loadImage kindBinary kindClusterName imageLoadSpec kindClusterEnvironment
      (KubernetesClusterMinikube {..}) ->
        -- Don't pass minikubeFlags; see comment above.
        Minikube.loadImage minikubeBinary kubernetesClusterName [] imageLoadSpec

        -- Because of the possible silent failure in "minikube image load", confirm that this
        -- image made it onto the cluster.
        -- At the moment this approach doesn't work, because if you do
        -- "minikube image load busybox:1.36.1-musl"
        -- followed by
        -- "minikube image ls",
        -- the result contains "docker.io/library/busybox:1.36.1-musl".
        -- Where did the docker.io/library/ come from? Need to understand this before we can
        -- check this properly.
        --
        -- image' <- Minikube.loadImage minikubeBinary kubernetesClusterName minikubeFlags image
        -- loadedImages <- Set.toList <$> getLoadedImages' kcc
        -- loadedImages `shouldContain` [image']
        -- return image'

-- | Same as 'withImageLoadRetry'', but with a reasonable default retry policy.
withImageLoadRetry :: (MonadLoggerIO m, MonadMask m) => ImageLoadSpec -> m a -> m a
withImageLoadRetry = withImageLoadRetry' (exponentialBackoff 50000 <> limitRetries 5)

-- | A combinator to wrap around your 'loadImage' or 'loadImageIfNecessary' calls to retry
-- on failure. Image loads sometimes fail on Minikube (version 1.33.0 at time of writing).
withImageLoadRetry' :: (MonadLoggerIO m, MonadMask m) => RetryPolicyM m -> ImageLoadSpec -> m a -> m a
withImageLoadRetry' policy ils action =
  recovering policy [\_status -> Handler (\(e :: FailureReason) -> do
                                             warn [i|#{ils}: retrying load due to exception: #{e}|]
                                             return True)] $ \_ ->
    action


-- | Helper to introduce a list of images into a Kubernetes cluster.
-- Stores the list of transformed image names under the "kubernetesClusterImages" label.
introduceImages :: (
  HasCallStack, MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context
  ) => [ImageLoadSpec] -> SpecFree (LabelValue "kubernetesClusterImages" [Text] :> context) m () -> SpecFree context m ()
introduceImages images = introduceWith "Introduce cluster images" kubernetesClusterImages $ \action ->
  forM images (\x -> loadImage x) >>= (void . action)
