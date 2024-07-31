{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Util.Images (
  dockerPullIfNecessary
  , isDockerImagePresent

  , podmanPullIfNecessary
  , isPodmanImagePresent

  , readImageName
  , readUncompressedImageName
  , imageLoadSpecToImageName
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Relude
import Safe
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Temporary


-- * Docker

-- | Pull an image using Docker if it isn't already present.
-- Returns 'True' if a pull was done.
dockerPullIfNecessary :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> ImagePullPolicy -> m Bool
dockerPullIfNecessary = commonPullIfNecessary "docker"

isDockerImagePresent :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> m Bool
isDockerImagePresent = isImagePresentCommon "docker"

-- * Podman

-- | Pull an image using Docker if it isn't already present.
-- Returns 'True' if a pull was done.
podmanPullIfNecessary :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> ImagePullPolicy -> m Bool
podmanPullIfNecessary = commonPullIfNecessary "podman"

isPodmanImagePresent :: (MonadUnliftIO m, MonadLoggerIO m) => Text -> m Bool
isPodmanImagePresent = isImagePresentCommon "podman"

-- * Common

commonPullIfNecessary :: (MonadUnliftIO m, MonadLoggerIO m) => String -> Text -> ImagePullPolicy -> m Bool
commonPullIfNecessary binary image pullPolicy = isImagePresentCommon binary image >>= \case
  True ->
    if | pullPolicy == Always -> doPull
       | otherwise -> return False
  False ->
    if | pullPolicy == Never -> expectationFailure [i|Docker pull policy was "Never" but image wasn't present: '#{image}'|]
       | otherwise -> doPull
  where
    doPull = do
      createProcessWithLogging (proc binary ["pull", toString image])
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)
      return True

isImagePresentCommon :: (MonadUnliftIO m, MonadLoggerIO m) => String -> Text -> m Bool
isImagePresentCommon binary image = do
  createProcessWithLogging (proc binary ["inspect", "--type=image", toString image]) >>= waitForProcess >>= \case
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- * Image name reading

readImageName :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => FilePath -> m Text
readImageName path = doesDirectoryExist path >>= \case
  True -> readUncompressedImageName path
  False -> case takeExtension path of
    ".tar" -> extractFromTarball
    ".gz" -> extractFromTarball
    _ -> expectationFailure [i|readImageName: unexpected extension in #{path}. Wanted .tar, .tar.gz, or uncompressed directory.|]
  where
    extractFromTarball = do
      files <- readCreateProcessWithLogging (proc "tar" ["tf", path]) ""
      manifestFileName <- case headMay [t | t <- T.words (toText files), "manifest.json" `T.isInfixOf` t] of
        Just f -> pure $ toString $ T.strip f
        Nothing -> expectationFailure [i|readImageName: couldn't find manifest file in #{path}|]

      withSystemTempDirectory "manifest.json" $ \dir -> do
        _ <- readCreateProcessWithLogging ((proc "tar" ["xvf", path, manifestFileName]) { cwd = Just dir }) ""
        liftIO (BL.readFile (dir </> "manifest.json")) >>= getImageNameFromManifestJson path

readUncompressedImageName :: (HasCallStack, MonadIO m) => FilePath -> m Text
readUncompressedImageName path = liftIO (BL.readFile (path </> "manifest.json")) >>= getImageNameFromManifestJson path

getImageNameFromManifestJson :: (HasCallStack, MonadIO m) => FilePath -> LByteString -> m Text
getImageNameFromManifestJson path contents = do
  case A.eitherDecode contents of
    Left err -> expectationFailure [i|Couldn't decode manifest.json: #{err}|]
    Right (A.Array entries) -> case concatMap getRepoTags entries of
      (x:_) -> pure x
      [] -> expectationFailure [i|Didn't find a repo tag for image at #{path}|]
    Right x -> expectationFailure [i|Unexpected manifest.json format: #{x}|]

  where
    getRepoTags :: A.Value -> [Text]
    getRepoTags (A.Object (aesonLookup "RepoTags" -> Just (A.Array repoItems))) = [t | A.String t <- V.toList repoItems]
    getRepoTags _ = []

imageLoadSpecToImageName :: (MonadUnliftIO m, MonadLogger m) => ImageLoadSpec -> m Text
imageLoadSpecToImageName (ImageLoadSpecTarball image) = readImageName image
imageLoadSpecToImageName (ImageLoadSpecDocker image _) = pure image
imageLoadSpecToImageName (ImageLoadSpecPodman image _) = pure image
