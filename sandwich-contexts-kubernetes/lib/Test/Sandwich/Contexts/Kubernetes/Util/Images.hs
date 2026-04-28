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
dockerPullIfNecessary :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m) => Text -> ImagePullPolicy -> m Bool
dockerPullIfNecessary = commonPullIfNecessary "docker"

isDockerImagePresent :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m) => Text -> m Bool
isDockerImagePresent = isImagePresentCommon "docker"

-- * Podman

-- | Pull an image using Docker if it isn't already present.
-- Returns 'True' if a pull was done.
podmanPullIfNecessary :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m) => Text -> ImagePullPolicy -> m Bool
podmanPullIfNecessary = commonPullIfNecessary "podman"

isPodmanImagePresent :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m) => Text -> m Bool
isPodmanImagePresent = isImagePresentCommon "podman"

-- * Common

commonPullIfNecessary :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m) => String -> Text -> ImagePullPolicy -> m Bool
commonPullIfNecessary binary image pullPolicy = isImagePresentCommon binary image >>= \case
  True ->
    if | pullPolicy == Always -> doPull
       | otherwise -> return False
  False ->
    if | pullPolicy == Never -> expectationFailure [i|Docker pull policy was "Never" but image wasn't present: '#{image}'|]
       | otherwise -> doPull
  where
    doPull = do
      createProcessWithFileLogging' (takeFileName binary <> "-pull") (proc binary ["pull", toString image])
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)
      return True

isImagePresentCommon :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m) => String -> Text -> m Bool
isImagePresentCommon binary image = do
  createProcessWithFileLogging' (takeFileName binary <> "-inspect") (proc binary ["inspect", "--type=image", toString image]) >>= waitForProcess >>= \case
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- * Image name reading

readImageName :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => FilePath -> m Text
readImageName path = doesDirectoryExist path >>= \case
  True -> readUncompressedImageName path
  False -> case takeExtension path of
    ".tar" -> extractFromTarball
    ".gz" -> extractFromTarball
    _ -> expectationFailure [i|readImageName: unexpected extension in #{path}. Wanted .tar, .tar.gz, or uncompressed directory.|]
  where
    extractFromTarball = do
      files <- readCreateProcessWithLogging (proc "tar" ["tf", path]) ""
      let fileList = T.words (toText files)
      case headMay [t | t <- fileList, "manifest.json" `T.isInfixOf` t] of
        Just manifestFile -> do
          let manifestFileName = toString $ T.strip manifestFile
          withSystemTempDirectory "manifest.json" $ \dir -> do
            _ <- readCreateProcessWithLogging ((proc "tar" ["xvf", path, manifestFileName]) { cwd = Just dir }) ""
            liftIO (BL.readFile (dir </> "manifest.json")) >>= getImageNameFromManifestJson path
        Nothing -> case headMay [t | t <- fileList, "index.json" `T.isInfixOf` t] of
          Just indexFile -> do
            let indexFileName = toString $ T.strip indexFile
            withSystemTempDirectory "index.json" $ \dir -> do
              _ <- readCreateProcessWithLogging ((proc "tar" ["xvf", path, indexFileName]) { cwd = Just dir }) ""
              liftIO (BL.readFile (dir </> "index.json")) >>= getImageNameFromOciIndex path
          Nothing -> expectationFailure [i|readImageName: couldn't find manifest.json or index.json in #{path}|]

readUncompressedImageName :: (HasCallStack, MonadIO m) => FilePath -> m Text
readUncompressedImageName path = do
  let manifestPath = path </> "manifest.json"
  let indexPath = path </> "index.json"
  liftIO (doesFileExist manifestPath) >>= \case
    True -> liftIO (BL.readFile manifestPath) >>= getImageNameFromManifestJson path
    False ->
      liftIO (doesFileExist indexPath) >>= \case
        True -> liftIO (BL.readFile indexPath) >>= getImageNameFromOciIndex path
        False -> expectationFailure [i|readUncompressedImageName: couldn't find manifest.json or index.json in #{path}|]

-- | Read image name from legacy Docker manifest.json
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

-- | Read image name from OCI index.json
getImageNameFromOciIndex :: (HasCallStack, MonadIO m) => FilePath -> LByteString -> m Text
getImageNameFromOciIndex path contents =
  case A.eitherDecode contents of
    Right (A.Object (aesonLookup "manifests" -> Just (A.Array manifests))) ->
      case headMay [name | A.Object (aesonLookup "annotations" -> Just (A.Object (aesonLookup "org.opencontainers.image.ref.name" -> Just (A.String name)))) <- V.toList manifests] of
        Just name -> pure name
        Nothing -> expectationFailure [i|Didn't find org.opencontainers.image.ref.name annotation in OCI index at #{path}|]
    Left err -> expectationFailure [i|Couldn't decode OCI index.json: #{err}|]
    Right x -> expectationFailure [i|Unexpected OCI index.json format: #{x}|]

imageLoadSpecToImageName :: (MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => ImageLoadSpec -> m Text
imageLoadSpecToImageName (ImageLoadSpecTarball image) = readImageName image
imageLoadSpecToImageName (ImageLoadSpecDocker image _) = pure image
imageLoadSpecToImageName (ImageLoadSpecPodman image _) = pure image
