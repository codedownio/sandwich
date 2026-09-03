{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images (
  getLoadedImagesMinikube
  , clusterContainsImageMinikube
  , loadImageMinikube
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Retry
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text as T
import Relude
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Images
import Text.Regex.TDFA
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Temporary


imageLoadRetryPolicy :: Monad m => RetryPolicyM m
imageLoadRetryPolicy = capDelay 15_000_000 (exponentialBackoff 1_000_000) <> limitRetries 5

-- | Load an image onto a cluster. This image can come from a variety of sources, as specified by the 'ImageLoadSpec'.
loadImageMinikube :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m
  )
  -- | Path to @minikube@ binary
  => FilePath
  -- | Cluster name
  -> Text
  -- | Extra flags to pass to @minikube@
  -> [Text]
  -- | Extra environment variables to add over the ambient environment (empty = inherit unchanged)
  -> [(String, String)]
  -- | Image load spec
  -> ImageLoadSpec
  -- | Returns transformed image name
  -> m Text
loadImageMinikube minikubeBinary clusterName minikubeFlags minikubeExtraEnv imageLoadSpec = do
  case imageLoadSpec of
    ImageLoadSpecTarball image -> do
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
              >>= \(ps, asy) -> finally (waitForProcess ps >>= (`shouldBe` ExitSuccess))
                                        (cancel asy)
            imageLoad tarFile False
            readImageName (toString image)
        False -> case takeExtension (toString image) of
          ".tar" -> do
            imageLoad (toString image) False
            readImageName (toString image)
          ".gz" -> do
            withSystemTempDirectory "image-tarball" $ \tempDir -> do
              let tarFile = tempDir </> "image.tar"
              -- TODO: don't depend on external gzip binary
              createProcessWithLogging (shell [i|cat "#{image}" | gzip -d > "#{tarFile}"|])
                >>= \(ps, asy) -> finally (waitForProcess ps >>= (`shouldBe` ExitSuccess))
                                          (cancel asy)
              imageLoad tarFile False
              readImageName (toString image)
          _ -> expectationFailure [i|Unexpected image extension in #{image}. Wanted .tar, .tar.gz, or uncompressed directory.|]

    ImageLoadSpecDocker image pullPolicy -> do
      _ <- dockerPullIfNecessary image pullPolicy
      imageLoad (toString image) True >> return image

    ImageLoadSpecPodman image pullPolicy -> do
      _ <- podmanPullIfNecessary image pullPolicy
      imageLoad (toString image) True >> return image

  where
    -- The usual reason a load fails is that containerd inside the node isn't serving yet:
    -- @ctr: cannot access socket /run/containerd/containerd.sock: no such file or directory@.
    -- That needs time rather than another immediate attempt, so back off between tries.
    imageLoad :: (MonadLoggerIO m, HasBaseContextMonad context m, HasCallStack) => String -> Bool -> m ()
    imageLoad toLoad daemon =
      retrying imageLoadRetryPolicy shouldRetry (const (imageLoadOnce toLoad daemon)) >>= \case
        Nothing -> return ()
        Just details -> expectationFailure [i|minikube image load failed; error output detected (#{details})|]
      where
        shouldRetry _ Nothing = return False
        shouldRetry _ (Just details) = do
          warn [i|minikube image load failed (#{details}); retrying|]
          return True

    imageLoadOnce :: (MonadLoggerIO m, HasBaseContextMonad context m, HasCallStack) => String -> Bool -> m (Maybe Text)
    imageLoadOnce toLoad daemon = do
      let extraFlags = case "--rootless" `L.elem` minikubeFlags of
                         True -> ["--rootless"]
                         False -> []

      let args = ["image", "load", toLoad
                 , "--profile", toString clusterName
                 , "--logtostderr=true", "--v=1"
                 , [i|--daemon=#{A.encode daemon}|]
                 ] <> extraFlags

      debug [i|#{minikubeBinary} #{T.unwords $ fmap toText args}|]

      procEnv <- minikubeProcEnv minikubeExtraEnv

      -- Gather stderr output while also logging it
      logFn <- askLoggerIO
      ctx <- ask
      stderrOutputVar <- newIORef mempty
      let customLogFn loc src level str = do
            modifyIORef' stderrOutputVar (<> str)
            logFn loc src level str

      exitCode <- liftIO $ flip runLoggingT customLogFn $ flip runReaderT ctx $
        createProcessWithLogging ((proc minikubeBinary args) { env = procEnv })
          >>= \(ps, asy) -> finally (waitForProcess ps) (cancel asy)

      stderrOutput <- fromLogStr <$> readIORef stderrOutputVar

      return $ failureReason exitCode stderrOutput

    failureReason :: ExitCode -> ByteString -> Maybe Text
    failureReason exitCode stderrOutput
      | exitCode /= ExitSuccess = Just [i|Exited with #{exitCode}|]
      | check1 stderrOutput = Just "Contained 'Failed to load cached images for profile' message"
      | check2 stderrOutput = Just "Contained 'ctr: failed to ingest' message"
      | check3 stderrOutput = Just "Contained 'failed pushing to' message"
      | otherwise = Nothing

    -- This is crazy, but minikube image load sometimes fails silently.
    -- One example: https://github.com/kubernetes/minikube/issues/16032
    -- As a result, we add a few checks to detect the cases we've seen that represent a failed load.

    check1 bytes = "Failed to load cached images for profile" `B.isInfixOf` bytes
                 && "make sure the profile is running." `B.isInfixOf` bytes

    check2 bytes = "ctr: failed to ingest" `B.isInfixOf` bytes
                 && "failed to copy: failed to send write: error reading from server: EOF: unavailable" `B.isInfixOf` bytes

    check3 :: ByteString -> Bool
    check3 bytes = bytes =~ ("failed pushing to:[[:blank:]]*[^[:space:]]+$" :: Text)

-- | Get the loaded images on a cluster, by cluster name.
getLoadedImagesMinikube :: (
  MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  )
  -- | Path to @minikube@ binary
  => FilePath
  -- | Cluster name
  -> Text
  -- | Extra flags to pass to @minikube@
  -> [Text]
  -- | Extra environment variables to add over the ambient environment (empty = inherit unchanged)
  -> [(String, String)]
  -> m (Set Text)
getLoadedImagesMinikube minikubeBinary clusterName minikubeFlags minikubeExtraEnv = do
  procEnv <- minikubeProcEnv minikubeExtraEnv
  -- TODO: use "--format json" and parse?
  (Set.fromList . T.words . toText) <$> readCreateProcessWithLogging (
    (proc minikubeBinary (["image", "ls"
                          , "--profile", toString clusterName
                          ] <> fmap toString minikubeFlags)) { env = procEnv }) ""

-- | Test if the cluster contains a given image, by cluster name.
clusterContainsImageMinikube :: (
  MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  )
  -- | Path to @minikube@ binary
  => FilePath
  -- | Cluster name
  -> Text
  -- | Extra flags to pass to @minikube@
  -> [Text]
  -- | Extra environment variables to add over the ambient environment (empty = inherit unchanged)
  -> [(String, String)]
  -- | Image name
  -> Text
  -> m Bool
clusterContainsImageMinikube minikubeBinary clusterName minikubeFlags minikubeExtraEnv image = do
  imageName <- case isAbsolute (toString image) of
    False -> pure image
    True -> readImageName (toString image)

  loadedImages <- getLoadedImagesMinikube minikubeBinary clusterName minikubeFlags minikubeExtraEnv

  return (
    imageName `Set.member` loadedImages

    -- Deal with weird prefixing Minikube does; see
    -- https://github.com/kubernetes/minikube/issues/19343
    || ("docker.io/" <> imageName) `Set.member` loadedImages
    || ("docker.io/library/" <> imageName) `Set.member` loadedImages
    )
