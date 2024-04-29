{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Relude
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Container
import UnliftIO.Directory
import UnliftIO.Process


introduceImages :: (
  MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context
  ) => [Text] -> SpecFree (LabelValue "kubernetesClusterImages" [Text] :> context) m () -> SpecFree context m ()
introduceImages images = introduceWith "introduce minikube cluster images" kubernetesClusterImages $ \action ->
  withLoadImages images $ \images' ->
    void $ action images'

withLoadImages :: (
  MonadUnliftIO m, MonadLogger m
  , MonadReader context m, HasBaseContext context, HasKubernetesClusterContext context
  ) => [Text] -> ([Text] -> m a) -> m a
withLoadImages images action = do
  kcc <- getContext kubernetesCluster
  withLoadImages' kcc images action

withLoadImages' :: (
  MonadUnliftIO m, MonadLogger m
  , MonadReader context m, HasBaseContext context
  ) => KubernetesClusterContext -> [Text] -> ([Text] -> m a) -> m a
withLoadImages' kcc@(KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterMinikube {..})}) images action = do
  let tweak image = "docker.io/" <> image

  let extraFlags = case "--rootless" `L.elem` minikubeFlags of
        True -> ["--rootless"]
        False -> []

  images' <- forM images $ \image -> do
    debug [i|Loading docker image '#{image}'|]
    timeAction [i|Loading docker image '#{image}'|] $ do
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
                         --profile #{kubernetesClusterName kcc}
                         --logtostderr
                         #{T.unwords extraFlags}
                         --alsologtostderr --v=2
                         |]
          debug [i|withLoadImages': #{cmd}|]
          createProcessWithLogging (shell cmd) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
          tweak <$> readImageName (toString image)

        False -> do
          let cmd = [iii|#{minikubeBinary} image load #{image}
                         --profile #{kubernetesClusterName kcc}
                         --logtostderr
                         --daemon=true
                         #{T.unwords extraFlags}
                         --alsologtostderr --v=2
                         |]
          debug [i|withLoadImages': #{cmd}|]
          createProcessWithLogging (shell cmd) >>= waitForProcess >>= (`shouldBe` ExitSuccess)
          return $ tweak image

  -- TODO: remove this?
  let cmd = [iii|#{minikubeBinary} image ls --profile #{kubernetesClusterName kcc}|]
  imageList <- readCreateProcessWithLogging (shell cmd) ""
  info [i|Loaded image list: #{imageList}|]

  action images'

withLoadImages' _ _images _action = error "Expected Minikube KubernetesClusterContext"
