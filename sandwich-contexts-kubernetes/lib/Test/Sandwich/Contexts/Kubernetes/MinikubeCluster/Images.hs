{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images (
  loadImage
  , getLoadedImages
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import Relude
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Util.Container
import UnliftIO.Directory
import UnliftIO.Process


loadImage :: (
  MonadUnliftIO m, MonadLogger m
  ) => FilePath -> Text -> [Text] -> Text -> m Text
loadImage minikubeBinary clusterName minikubeFlags image = do
  let tweak = ("docker.io/" <>)

  let extraFlags = case "--rootless" `L.elem` minikubeFlags of
        True -> ["--rootless"]
        False -> []

  case isAbsolute (toString image) of
    True -> do
      initialStream :: Text <- doesDirectoryExist (toString image) >>= \case
        True ->
          -- Uncompressed directory: tar it up (but don't zip)
          pure [i|tar -C "#{image}" --dereference --hard-dereference --xform s:'^./':: -c .|]
        False -> case takeExtension (toString image) of
          ".tar" -> pure [i|cat "#{image}"|]
          ".gz" -> pure [i|cat "#{image}" | gzip -d|]
          _ -> expectationFailure [i|Unexpected image extension in #{image}. Wanted .tar, .tar.gz, or uncompressed directory.|]

      let cmd = [iii|#{initialStream} | #{minikubeBinary} image load -
                     --profile #{clusterName}
                     --logtostderr
                     #{T.unwords extraFlags}
                     --alsologtostderr --v=2
                     |]
      debug [i|loadImages': #{cmd}|]
      createProcessWithLogging (shell cmd) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
      tweak <$> readImageName (toString image)

    False -> do
      let cmd = [iii|#{minikubeBinary} image load #{image}
                     --profile #{clusterName}
                     --logtostderr
                     --daemon=true
                     #{T.unwords extraFlags}
                     --alsologtostderr --v=2
                     |]
      debug [i|loadImages': #{cmd}|]
      createProcessWithLogging (shell cmd) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
      return $ tweak image

getLoadedImages :: (MonadUnliftIO m, MonadLogger m) => FilePath -> Text -> [Text] -> m (Set Text)
getLoadedImages minikubeBinary clusterName minikubeFlags = do
  -- TODO: use "--format json" and parse?
  (Set.fromList . T.words . toText) <$> readCreateProcessWithLogging (
    proc minikubeBinary (["image", "ls"
                         , "--profile", toString clusterName
                         ] <> fmap toString minikubeFlags)) ""
