{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Sandwich.Contexts.Kubernetes.SeaweedFS (
  introduceSeaweedFSCluster
  , withSeaweedFS
  , withSeaweedFS'

  , SeaweedFSOptions(..)
  , defaultSeaweedFSOptions

  , seaweedFs
  , SeaweedFSContext(..)
  , HasSeaweedFSContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson as A
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Vector as V
import Data.Yaml as Yaml
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import UnliftIO.Environment
import UnliftIO.IO (withFile)
import UnliftIO.Process
import UnliftIO.Temporary


introduceSeaweedFSCluster :: (
  HasBaseContext context, HasKubernetesClusterContext context, MonadUnliftIO m
  ) =>Text -> SeaweedFSOptions -> SpecFree (LabelValue "seaweedFs" SeaweedFSContext :> context) m () -> SpecFree context m ()
introduceSeaweedFSCluster namespace options = introduceWith "introduce SeaweedFS" seaweedFs (void . withSeaweedFS namespace options)

withSeaweedFS :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m, HasBaseContextMonad context m, HasKubernetesClusterContext context
  ) => Text -> SeaweedFSOptions -> (SeaweedFSContext -> m a) -> m a
withSeaweedFS namespace options action = do
  kcc <- getContext kubernetesCluster
  withSeaweedFS' kcc namespace options action

withSeaweedFS' :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m, HasBaseContextMonad context m
  ) => KubernetesClusterContext -> Text -> SeaweedFSOptions -> (SeaweedFSContext -> m a) -> m a
withSeaweedFS' kcc@(KubernetesClusterContext {kubernetesClusterKubeConfigPath}) namespace options action = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  let cp = proc "nix" ["build", "--impure", "--expr", [__i|with import <nixpkgs> {}; fetchFromGitHub {
                                                             owner = "seaweedfs";
                                                             repo = "seaweedfs-operator";
                                                             rev = "1cb1ee5a78e68cfa2d83d00813f50fd98d7b1092";
                                                             sha256 = "sha256-p2SNwwwJ0GrmkImdW1aUYAluDLuY86FtV1F21xUHnyk=";
                                                           }|], "--json"]
  operatorJson <- withFile "/dev/null" WriteMode $ \hNull ->
    readCreateProcess (cp { std_err = UseHandle hNull }) ""

  operatorPath <- case A.eitherDecodeStrict (encodeUtf8 operatorJson) of
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String p))))):_))) -> pure p
    x -> expectationFailure [i|Couldn't parse seaweedfs-operator path: #{x}|]

  info [i|Got operator path: #{operatorPath}|]

  withSystemTempDirectory "seaweedfs-operator" $ \dir -> do
    let target = dir </> "seaweefs-operator"
    _ <- readCreateProcess (proc "cp" ["-r", toString operatorPath, target]) ""
    _ <- readCreateProcess (proc "chmod" ["-R", "u+w", target]) ""

    let runOperatorCmd cmd extraEnv = createProcessWithLogging (
          (shell cmd) {
              env = Just (env <> extraEnv)
              , cwd = Just target
              }
          ) >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    info [i|------------------ Building and uploading SeaweedFS Docker image ------------------|]

    info [i|Doing make docker-build|]
    runOperatorCmd "make docker-build" []

    withLoadImages' kcc ["chrislusf/seaweedfs-operator:v0.0.1"] $ \[newImageName] -> do
      info [i|------------------ Installing SeaweedFS operator ------------------|]

      info [i|Doing make install|]
      runOperatorCmd "make install" [("IMG", toString newImageName)]
      info [i|Doing make deploy|]
      runOperatorCmd "make deploy" [("IMG", toString newImageName)]

      info [i|------------------ Creating SeaweedFS deployment ------------------|]

      let val = decodeUtf8 $ A.encode $ example namespace options
      createProcessWithLoggingAndStdin ((shell "kubectl create -f -") { env = Just env }) val >>= waitForProcess >>= (`shouldBe` ExitSuccess)

      action $ SeaweedFSContext {
        seaweedFsOptions = options
        }


example :: Text -> SeaweedFSOptions -> Yaml.Value
example namespace (SeaweedFSOptions {..}) = let Right x = Yaml.decodeEither' raw in x
 where raw = [i|apiVersion: seaweed.seaweedfs.com/v1
kind: Seaweed
metadata:
  namespace: #{namespace}
  name: #{seaweedFsBaseName}
spec:
  image: #{seaweedFsImage}
  volumeServerDiskCount: #{seaweedFsVolumeServerDiskCount}
  hostSuffix: seaweed.abcdefg.com
  master:
    replicas: #{seaweedFsMasterReplicas}
    volumeSizeLimitMB: #{seaweedFsVolumeSizeLimitMb}
  volume:
    replicas: #{seaweedFsVolumeReplicas}
    requests:
      storage: #{seaweedFsVolumeStorageRequest}
  filer:
    replicas: #{seaweedFsFilerReplicas}
    config: |
      [leveldb2]
      enabled = true
      dir = "/data/filerldb2"
|]
