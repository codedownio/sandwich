{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.Kubernetes.KindCluster.Images where

import Sandwich.Contexts.Kubernetes.Types
import Sandwich.Contexts.Kubernetes.Util.Container
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text as T
import Relude
import System.Exit
import System.FilePath
import Test.Sandwich
import UnliftIO.Process
import UnliftIO.Temporary


withLoadImages :: (
  MonadUnliftIO m, MonadBaseControl IO m, MonadLogger m, MonadReader context m, HasBaseContext context, HasKubernetesClusterContext context
  ) => [Text] -> Maybe [(String, String)] -> ([Text] -> m a) -> m a
withLoadImages images env action = do
  kcc <- getContext kubernetesCluster
  withLoadImages' kcc env images action

withLoadImages' :: (
  MonadUnliftIO m, MonadBaseControl IO m, MonadLogger m, MonadReader context m, HasBaseContext context
  ) => KubernetesClusterContext -> Maybe [(String, String)] -> [Text] -> ([Text] -> m a) -> m a
withLoadImages' kcc env images action = do
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
