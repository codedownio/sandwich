{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Sandwich.Contexts.Kubernetes.SeaweedFS (
  introduceSeaweedFS
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
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml as Yaml
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Images (loadImage')
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import Test.Sandwich.Contexts.Nix
import UnliftIO.Environment
import UnliftIO.IO (withFile)
import UnliftIO.Process
import UnliftIO.Temporary


data SeaweedFSContext = SeaweedFSContext {
  seaweedFsOptions :: SeaweedFSOptions
  } deriving (Show)

data SeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage :: Text
  , seaweedFsBaseName :: Text
  , seaweedFsMasterReplicas :: Int
  , seaweedFsFilerReplicas :: Int
  , seaweedFsVolumeReplicas :: Int
  , seaweedFsVolumeServerDiskCount :: Int
  , seaweedFsVolumeSizeLimitMb :: Int
  , seaweedFsVolumeStorageRequest :: Text
  } deriving (Show)
defaultSeaweedFSOptions :: SeaweedFSOptions
defaultSeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage = "chrislusf/seaweedfs:3.73"
  , seaweedFsBaseName = "seaweed1"
  , seaweedFsMasterReplicas = 3
  , seaweedFsFilerReplicas = 2
  , seaweedFsVolumeReplicas = 1
  , seaweedFsVolumeServerDiskCount = 1
  , seaweedFsVolumeSizeLimitMb = 1024
  , seaweedFsVolumeStorageRequest = "2Gi"
  }

seaweedFs :: Label "seaweedFs" SeaweedFSContext
seaweedFs = Label
type HasSeaweedFSContext context = HasLabel context "seaweedFs" SeaweedFSContext

introduceSeaweedFS :: (
  MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context, HasNixContext context
  ) =>Text -> SeaweedFSOptions -> SpecFree (LabelValue "seaweedFs" SeaweedFSContext :> LabelValue "file-kubectl" (EnvironmentFile "kubectl") :> context) m () -> SpecFree context m ()
introduceSeaweedFS namespace options = introduceBinaryViaNixPackage @"kubectl" "kubectl" . introduceWith "introduce SeaweedFS" seaweedFs (void . withSeaweedFS namespace options)

withSeaweedFS :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context, HasNixContext context, HasFile context "kubectl"
  ) => Text -> SeaweedFSOptions -> (SeaweedFSContext -> m a) -> m a
withSeaweedFS namespace options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withSeaweedFS' kcc kubectlBinary namespace options action

withSeaweedFS' :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  , HasBaseContextMonad context m, HasNixContext context
  ) => KubernetesClusterContext -> FilePath -> Text -> SeaweedFSOptions -> (SeaweedFSContext -> m a) -> m a
withSeaweedFS' kcc@(KubernetesClusterContext {kubernetesClusterKubeConfigPath}) kubectlBinary namespace options action = do
  baseEnv <- getEnvironment

  NixContext {..} <- getContext nixContext

  let cp = proc nixContextNixBinary ["build", "--impure"
                                    , "--extra-experimental-features", "nix-command"
                                    , "--expr", seaweedFsOperatorDerivation
                                    , "--json"]

  operatorJson <- withFile "/dev/null" WriteMode $ \hNull ->
    readCreateProcess (cp { std_err = UseHandle hNull }) ""

  operatorPath <- case A.eitherDecodeStrict (encodeUtf8 operatorJson) of
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String p))))):_))) -> pure p
    x -> expectationFailure [i|Couldn't parse seaweedfs-operator path: #{x}|]

  info [i|Got operator path: #{operatorPath}|]

  -- Build a Nix environment with some tools needed by the operator
  nixEnvPath <- buildNixSymlinkJoin ["coreutils", "gnumake", "go", "stdenv", "which"]
  info [i|Built Nix environment for operator builds: #{nixEnvPath}|]

  let originalSearchPathParts = maybe [] splitSearchPath (L.lookup "PATH" baseEnv)
  let finalPath = (nixEnvPath </> "bin") : takeDirectory kubectlBinary : originalSearchPathParts
                & fmap toText
                & T.intercalate (toText [searchPathSeparator])
                & toString

  let env = baseEnv
          & (("KUBECONFIG", kubernetesClusterKubeConfigPath) :)
          & (("PATH", finalPath) :)
          & L.nubBy (\x y -> fst x == fst y)

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

    let initialImageName = "seaweedfs/seaweedfs-operator:v0.0.1"

    info [i|Doing make docker-build|]
    runOperatorCmd "make docker-build" [("IMG", toString initialImageName)]

    newImageName <- loadImage' kcc (ImageLoadSpecDocker initialImageName IfNotPresent)
    info [i|Loaded image into cluster as: #{newImageName}|]

    info [i|------------------ Installing SeaweedFS operator ------------------|]

    info [i|Doing make install|]
    runOperatorCmd "make install" [("IMG", toString newImageName)]
    info [i|Doing make deploy|]
    runOperatorCmd "make deploy" [("IMG", toString newImageName)]

    info [i|------------------ Creating SeaweedFS deployment ------------------|]

    let val = decodeUtf8 $ A.encode $ example namespace options
    createProcessWithLoggingAndStdin ((shell [i|#{kubectlBinary} create -f -|]) { env = Just env }) val
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)

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

seaweedFsOperatorDerivation = [__i|with import <nixpkgs> {}; fetchFromGitHub {
                                     owner = "seaweedfs";
                                     repo = "seaweedfs-operator";
                                     rev = "6fa4c24d47c57daa10a084e3a5598efbb8d808c8";
                                     sha256 = "sha256-gFFIG2tglzvXoqzUvbzWAG2Bg2RwCCsuX0tXwV95D/0=";
                                   }
                                  |]
