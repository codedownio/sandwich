{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.Images (
  getLoadedImages
  , clusterContainsImage
  , loadImage
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson as A
import qualified Data.Set as Set
import Data.String.Interpolate
import qualified Data.Vector as V
import Relude
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Setup
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import Test.Sandwich.Contexts.Kubernetes.Util.Images
import UnliftIO.Process
import UnliftIO.Temporary


loadImage :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m
  )
  -- | Kind binary
  => FilePath
  -- | Cluster name
  -> Text
  -- | Image name
  -> ImageLoadSpec
  -- | Environment variables
  -> Maybe [(String, String)]
  -- | Callback with transformed image names (see above)
  -> m Text
loadImage kindBinary clusterName imageLoadSpec env = do
  case imageLoadSpec of
    ImageLoadSpecTarball image -> do
      withSystemTempDirectory "kind-image-zip" $ \dir -> do
        let archive = dir </> "test.tar"
        _ <- readCreateProcessWithLogging (shell [i|tar -C #{image} --dereference --hard-dereference --xform s:'^./':: -c . > #{archive}|]) ""

        debug [i|Made image archive: #{archive}|]
        createProcessWithLogging (
          (shell [i|#{kindBinary} load image-archive #{archive} --name #{clusterName}|]) {
              env = env
              }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
        readUncompressedImageName (toString image)
    ImageLoadSpecDockerImage image pullPolicy -> do
      _ <- dockerPullIfNecessary image pullPolicy

      createProcessWithLogging (
        (shell [i|#{kindBinary} load docker-image #{image} --name #{clusterName}|]) {
            env = env
            }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)

      return image
    ImageLoadSpecPodmanImage image pullPolicy -> do
      _ <- podmanPullIfNecessary image pullPolicy

      _ <- expectationFailure [i|Not implemented yet.|]

      return image


getLoadedImages :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m
  ) => KubernetesClusterContext -> Text -> FilePath -> Maybe [(String, String)] -> m (Set Text)
getLoadedImages kcc driver kindBinary env = do
  chosenNode <- getNodes kcc kindBinary env >>= \case
    (x:_) -> pure x
    [] -> expectationFailure [i|Couldn't identify a Kind node.|]

  output <- readCreateProcessWithLogging (
    (proc (toString driver) [
        "exec"
        , toString chosenNode
        , "crictl", "images", "-o", "json"
        ]) { env = env }
    ) ""

  case A.eitherDecode (encodeUtf8 output) of
    Left err -> expectationFailure [i|Couldn't decode JSON (#{err}): #{output}|]
    Right (A.Object (aesonLookup "images" -> Just (A.Array images))) -> return $ Set.fromList $ concatMap extractRepoTags images
    _ -> expectationFailure [i|Unexpected format in JSON: #{output}|]

  where
    extractRepoTags :: A.Value -> [Text]
    extractRepoTags (A.Object (aesonLookup "repoTags" -> Just (A.Array xs))) = [t | A.String t <- V.toList xs]
    extractRepoTags _ = []

clusterContainsImage :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m
  ) => KubernetesClusterContext -> Text -> FilePath -> Maybe [(String, String)] -> Text -> m Bool
clusterContainsImage kcc driver kindBinary env image = do
  imageName <- case isAbsolute (toString image) of
    False -> pure image
    True -> readImageName (toString image)

  loadedImages <- getLoadedImages kcc driver kindBinary env

  return (
    imageName `Set.member` loadedImages

    -- Deal with weird prefixing Minikube does; see
    -- https://github.com/kubernetes/minikube/issues/19343
    || ("docker.io/" <> imageName) `Set.member` loadedImages
    || ("docker.io/library/" <> imageName) `Set.member` loadedImages
    )
