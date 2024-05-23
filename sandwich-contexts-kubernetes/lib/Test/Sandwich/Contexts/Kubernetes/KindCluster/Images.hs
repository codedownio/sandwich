{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.Images where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Text as T
import Relude
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Container
import UnliftIO.Process
import UnliftIO.Temporary


-- | Bracket-style function to load a collection of images into a Kubernetes cluster.
withLoadImages :: (
  MonadUnliftIO m, MonadLogger m, MonadReader context m, HasBaseContext context, HasKubernetesClusterContext context
  )
  -- | Image names
  => [Text]
  -- | Optional environment variables to provide when calling "kind load"
  -> Maybe [(String, String)]
  -- | Callback containing the transformed image names (i.e. what they're known as within the cluster)
  -> ([Text] -> m a)
  -> m a
withLoadImages images env action = do
  kcc <- getContext kubernetesCluster
  withLoadImages' kcc images env action

-- | Same as 'withLoadImages', but allows you to pass in the 'KubernetesClusterContext', rather than requiring one in context.
withLoadImages' :: (
  MonadUnliftIO m, MonadLogger m, MonadReader context m, HasBaseContext context
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Image names
  -> [Text]
  -- | Environment variables
  -> Maybe [(String, String)]
  -- | Callback with transformed image names (see above)
  -> ([Text] -> m a)
  -> m a
withLoadImages' kcc images env action = do
  let tweak image = "docker.io/" <> image

  images' <- forM images $ \image -> do
    debug [i|Loading container image '#{image}'|]
    timeAction [i|Loading container image '#{image}'|] $ do
      case isAbsolute (toString image) of
        True -> do
          withSystemTempDirectory "kind-image-zip" $ \dir -> do
            let archive = dir </> "test.tar"
            _ <- readCreateProcessWithLogging (shell [i|tar -C #{image} --dereference --hard-dereference --xform s:'^./':: -c . > #{archive}|]) ""

            debug [i|Made image archive: #{archive}|]
            createProcessWithLogging (
              (shell [i|kind load image-archive #{archive} --name #{kubernetesClusterName kcc}|]) {
                  env = env
                  }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
            tweak <$> readUncompressedImageName (toString image)
        False -> do
          createProcessWithLogging (
            (shell [i|kind load docker-image #{image} --name #{kubernetesClusterName kcc}|]) {
                env = env
                }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
          return $ tweak image

  action images'
