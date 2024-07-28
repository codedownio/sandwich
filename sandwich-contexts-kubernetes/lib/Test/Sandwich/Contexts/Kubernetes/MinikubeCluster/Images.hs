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
import UnliftIO.Temporary


loadImage :: (
  MonadUnliftIO m, MonadLogger m, MonadFail m, MonadReader context m, HasBaseContext context
  ) => FilePath -> Text -> [Text] -> Text -> m Text
loadImage minikubeBinary clusterName minikubeFlags image = do
  Just dir <- getCurrentFolder

  case isAbsolute (toString image) of
    True -> do
      -- File or directory image
      doesDirectoryExist (toString image) >>= \case
        True ->
          -- Uncompressed directory: tar it up (but don't zip).
          -- Formerly we would execute a shell with a pipe to direct the tar output directly into "minikube image load".
          -- But then "minikube image load" would just write its own tarball in /tmp, like /tmp/build.12345.tar, and
          -- leave it there!
          withSystemTempDirectory "image-tarball" $ \tempDir -> do
            let tarFile = tempDir </> "image.tar"
            -- TODO: don't depend on external tar file
            createProcessWithLogging (shell [i|tar -C "#{image}" --dereference --hard-dereference --xform s:'^./':: -c . > "#{tarFile}"|])
              >>= waitForProcess >>= (`shouldBe` ExitSuccess)
            imageLoad tarFile
            readImageName (toString image)
        False -> case takeExtension (toString image) of
          ".tar" -> do
            imageLoad (toString image)
            readImageName (toString image)
          ".gz" -> do
            withSystemTempDirectory "image-tarball" $ \tempDir -> do
              let tarFile = tempDir </> "image.tar"
              -- TODO: don't depend on external gzip file
              createProcessWithLogging (shell [i|cat "#{image}" | gzip -d > "#{tarFile}"|])
                >>= waitForProcess >>= (`shouldBe` ExitSuccess)
              imageLoad tarFile
              readImageName (toString image)
          _ -> expectationFailure [i|Unexpected image extension in #{image}. Wanted .tar, .tar.gz, or uncompressed directory.|]

    False ->
      -- Docker/Podman image
      imageLoad (toString image) >> return image

  where
    imageLoad toLoad = do
      let extraFlags = case "--rootless" `L.elem` minikubeFlags of
                         True -> ["--rootless"]
                         False -> []

      let args = ["image", "load", toLoad
                 , "--profile", toString clusterName
                 , "--logtostderr=true", "--v=2"
                 , "--daemon=true"
                 ] <> extraFlags

      debug [i|#{minikubeBinary} #{T.unwords $ fmap toText args}|]

      createProcessWithLogging (proc minikubeBinary args)
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

getLoadedImages :: (MonadUnliftIO m, MonadLogger m) => FilePath -> Text -> [Text] -> m (Set Text)
getLoadedImages minikubeBinary clusterName minikubeFlags = do
  -- TODO: use "--format json" and parse?
  (Set.fromList . T.words . toText) <$> readCreateProcessWithLogging (
    proc minikubeBinary (["image", "ls"
                         , "--profile", toString clusterName
                         ] <> fmap toString minikubeFlags)) ""
