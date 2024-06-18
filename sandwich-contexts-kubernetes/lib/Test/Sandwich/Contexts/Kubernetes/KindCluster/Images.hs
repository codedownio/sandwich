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
import Test.Sandwich.Contexts.Kubernetes.Util.Container
import UnliftIO.Process
import UnliftIO.Temporary


loadImage :: (
  MonadUnliftIO m, MonadLogger m
  )
  -- | Kind binary
  => FilePath
  -- | Cluster name
  -> Text
  -- | Image name
  -> Text
  -- | Environment variables
  -> Maybe [(String, String)]
  -- | Callback with transformed image names (see above)
  -> m Text
loadImage kindBinary clusterName image env = do
  let tweak = ("docker.io/" <>)

  case isAbsolute (toString image) of
    True -> do
      withSystemTempDirectory "kind-image-zip" $ \dir -> do
        let archive = dir </> "test.tar"
        _ <- readCreateProcessWithLogging (shell [i|tar -C #{image} --dereference --hard-dereference --xform s:'^./':: -c . > #{archive}|]) ""

        debug [i|Made image archive: #{archive}|]
        createProcessWithLogging (
          (shell [i|#{kindBinary} load image-archive #{archive} --name #{clusterName}|]) {
              env = env
              }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
        tweak <$> readUncompressedImageName (toString image)
    False -> do
      createProcessWithLogging (
        (shell [i|#{kindBinary} load docker-image #{image} --name #{clusterName}|]) {
            env = env
            }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
      return $ tweak image
