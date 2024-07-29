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
import Test.Sandwich.Contexts.Kubernetes.Util.Container
import Text.Regex.TDFA
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Temporary


loadImage :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadFail m
  ) => FilePath -> Text -> [Text] -> Text -> m Text
loadImage minikubeBinary clusterName minikubeFlags image = do
  -- Don't know why this is needed. But if you load an image like codedown/server:abcdef,
  -- Minikube will seemingly prepend docker.io/.
  -- We probably need to prepend our images with own own domain name to change this.
  let tweak = ("docker.io/" <>)

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
            imageLoad tarFile False
            tweak <$> readImageName (toString image)
        False -> case takeExtension (toString image) of
          ".tar" -> do
            imageLoad (toString image) False
            tweak <$> readImageName (toString image)
          ".gz" -> do
            withSystemTempDirectory "image-tarball" $ \tempDir -> do
              let tarFile = tempDir </> "image.tar"
              -- TODO: don't depend on external gzip file
              createProcessWithLogging (shell [i|cat "#{image}" | gzip -d > "#{tarFile}"|])
                >>= waitForProcess >>= (`shouldBe` ExitSuccess)
              imageLoad tarFile False
              tweak <$> readImageName (toString image)
          _ -> expectationFailure [i|Unexpected image extension in #{image}. Wanted .tar, .tar.gz, or uncompressed directory.|]

    False ->
      -- Docker/Podman image
      imageLoad (toString image) True >> return (tweak image)

  where
    imageLoad :: (MonadLoggerIO m, HasCallStack) => String -> Bool -> m ()
    imageLoad toLoad daemon = do
      let extraFlags = case "--rootless" `L.elem` minikubeFlags of
                         True -> ["--rootless"]
                         False -> []

      let args = ["image", "load", toLoad
                 , "--profile", toString clusterName
                 , "--logtostderr=true", "--v=2"
                 , [i|--daemon=#{A.encode daemon}|]
                 ] <> extraFlags

      debug [i|#{minikubeBinary} #{T.unwords $ fmap toText args}|]

      -- Gather stderr output while also logging it
      logFn <- askLoggerIO
      stderrOutputVar <- newIORef mempty
      let customLogFn loc src level str = do
            modifyIORef' stderrOutputVar (<> str)
            logFn loc src level str

      liftIO $ flip runLoggingT customLogFn $
        createProcessWithLogging (proc minikubeBinary args)
          >>= waitForProcess >>= (`shouldBe` ExitSuccess)

      stderrOutput <- fromLogStr <$> readIORef stderrOutputVar
      when (or [f stderrOutput | f <- badOutputChecks]) $
        expectationFailure [i|minikube image load failed; error output detected|]

    -- This is crazy, but minikube image load sometimes fails silently.
    -- One example: https://github.com/kubernetes/minikube/issues/16032
    -- As a result, we add a few checks to detect the cases we've seen that represent a failed load.
    badOutputChecks :: [ByteString -> Bool]
    badOutputChecks = [check1, check2, check3]

    check1 bytes = "Failed to load cached images for profile" `B.isInfixOf` bytes
                 && "make sure the profile is running." `B.isInfixOf` bytes

    check2 bytes = "ctr: failed to ingest" `B.isInfixOf` bytes
                 && "failed to copy: failed to send write: error reading from server: EOF: unavailable" `B.isInfixOf` bytes

    check3 :: ByteString -> Bool
    check3 bytes = bytes =~ ("failed pushing to:[[:blank:]]*[^[:space:]]+" :: Text)

getLoadedImages :: (MonadUnliftIO m, MonadLogger m) => FilePath -> Text -> [Text] -> m (Set Text)
getLoadedImages minikubeBinary clusterName minikubeFlags = do
  -- TODO: use "--format json" and parse?
  (Set.fromList . T.words . toText) <$> readCreateProcessWithLogging (
    proc minikubeBinary (["image", "ls"
                         , "--profile", toString clusterName
                         ] <> fmap toString minikubeFlags)) ""
