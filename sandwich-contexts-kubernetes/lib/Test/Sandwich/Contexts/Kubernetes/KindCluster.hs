{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster (
  introduceKindClusterViaNix
  , introduceKindClusterViaEnvironment
  , introduceKindCluster'

  -- * Bracket-style versions
  , withKindCluster
  , withKindCluster'

  -- * Loading images
  , withLoadImages
  , withLoadImages'

  -- * Re-exported types
  , KubernetesClusterContext (..)
  , kubernetesCluster
  , HasKubernetesClusterContext

  -- * Types
  , KindClusterOptions (..)
  , KindClusterName(..)
  , ExtraPortMapping(..)
  , ExtraMount(..)
  , defaultKindClusterOptions
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Yaml as Yaml
import Kubernetes.Client.Config
import Relude
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Config
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Images
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Setup
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Container (isInContainer)
import Test.Sandwich.Contexts.Kubernetes.Util.UUID
import Test.Sandwich.Contexts.Nix
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


-- Note: when using kind with podman as a driver, it's possible to run into a low PID limit
-- which isn't enough for all the processes in a Kubernetes cluster.
-- I debugged this and found a kind patch to fix it, described here:
-- https://github.com/kubernetes-sigs/kind/issues/3451#issuecomment-1855701939
--
-- You can also fix this at the podman level, with the following in `containers.conf`:
-- [containers]
-- pids_limit = 0


data KindClusterName =
  -- | Give the kind cluster an exact name
  KindClusterNameExactly Text
  -- | Autogenerate the cluster name, with an optional fixed prefix
  | KindClusterNameAutogenerate (Maybe Text)
  deriving (Show, Eq)

data KindClusterOptions = KindClusterOptions {
  kindClusterNumNodes :: Int
  -- | Extra flags to pass to kind
  , kindClusterExtraFlags :: [Text]
  -- | Labels to apply to the created containers
  , kindClusterContainerLabels :: Map Text Text
  -- | Extra ports to map; see the [docs](https://kind.sigs.k8s.io/docs/user/configuration#extra-port-mappings)
  , kindClusterExtraPortMappings :: [ExtraPortMapping]
  -- | Extra mounts; see the [docs](https://kind.sigs.k8s.io/docs/user/configuration#extra-mounts)
  , kindClusterExtraMounts :: [ExtraMount]
  -- | Prefix for the generated cluster name
  , kindClusterName :: KindClusterName
  -- | Container driver, either "docker" or "podman". Defaults to "docker"
  , kindClusterDriver :: Maybe Text
  -- , kindClusterCpus :: Maybe Text
  -- , kindClusterMemory :: Maybe Text
  }
defaultKindClusterOptions :: KindClusterOptions
defaultKindClusterOptions = KindClusterOptions {
  kindClusterNumNodes = 3
  , kindClusterExtraFlags = []
  , kindClusterContainerLabels = mempty
  , kindClusterExtraPortMappings = []
  , kindClusterExtraMounts = []
  , kindClusterName = KindClusterNameAutogenerate Nothing
  , kindClusterDriver = Nothing
  -- , kindClusterCpus = Nothing
  -- , kindClusterMemory = Nothing
  }

-- * Introduce


-- | Introduce a Kubernetes cluster using [kind](https://kind.sigs.k8s.io/), deriving the kind binary from the Nix context.
introduceKindClusterViaNix :: (
  HasBaseContext context, MonadUnliftIO m, MonadMask m, HasNixContext context
  )
  -- | Options
  => KindClusterOptions
  -- | Child spec
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> LabelValue "file-kind" (EnvironmentFile "kind") :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceKindClusterViaNix kindClusterOptions spec =
  introduceBinaryViaNixPackage @"kind" "kind" $
    introduceWith "introduce kind cluster" kubernetesCluster (void . withKindCluster kindClusterOptions) spec

introduceKindClusterViaEnvironment :: (
  HasBaseContext context, MonadMask m, MonadUnliftIO m
  )
  -- | Options
  => KindClusterOptions
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> LabelValue "file-kind" (EnvironmentFile "kind") :> context) m ()
  -> SpecFree context m ()
introduceKindClusterViaEnvironment kindClusterOptions spec =
  introduceBinaryViaEnvironment @"kind" $
    introduceWith "introduce kind cluster" kubernetesCluster (void . withKindCluster kindClusterOptions) spec

introduceKindCluster' :: (
  HasBaseContext context, MonadMask m, MonadUnliftIO m
  )
  -- | Path to kind binary
  => FilePath
  -> KindClusterOptions
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> LabelValue "file-kind" (EnvironmentFile "kind") :> context) m ()
  -> SpecFree context m ()
introduceKindCluster' kindBinary kindClusterOptions spec =
  introduceFile @"kind" kindBinary $
    introduceWith "introduce kind cluster" kubernetesCluster (void . withKindCluster kindClusterOptions) $
      spec

-- * Implementation

-- | Bracket-style variant of 'introduceKindCluster'.
withKindCluster :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadMask m, MonadFail m
  , HasBaseContextMonad context m, HasFile context "kind"
  )
  -- | Options
  => KindClusterOptions
  -> (KubernetesClusterContext -> m a)
  -> m a
withKindCluster opts action = do
  kindBinary <- askFile @"kind"
  withKindCluster' kindBinary opts action

-- | Same as 'withKindCluster', but allows you to pass in the path to the kind binary.
withKindCluster' :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadMask m, MonadFail m
  , HasBaseContextMonad context m
  ) => FilePath -> KindClusterOptions -> (KubernetesClusterContext -> m a) -> m a
withKindCluster' kindBinary opts@(KindClusterOptions {..}) action = do
  clusterName <- case kindClusterName of
    KindClusterNameExactly t -> pure t
    KindClusterNameAutogenerate maybePrefix -> do
      let prefix = fromMaybe "test-kind-cluster" maybePrefix
      clusterID <- makeUUID' 5
      return [i|#{prefix}-#{clusterID}|]

  kc <- isInContainer >>= \case
    False -> return $ kindConfig kindClusterNumNodes kindClusterContainerLabels kindClusterExtraPortMappings kindClusterExtraMounts
    True -> return $ kindConfig kindClusterNumNodes kindClusterContainerLabels kindClusterExtraPortMappings kindClusterExtraMounts

  Just dir <- getCurrentFolder
  kindConfigFile <- liftIO $ writeTempFile dir "kind-config" (decodeUtf8 $ Yaml.encode kc)
  info [i|kindConfigFile: #{kindConfigFile}|]

  kindKubeConfigFile <- liftIO $ writeTempFile dir "kind-kube-config" ""

  environmentToUse <- case kindClusterDriver of
    Just "docker" -> return Nothing
    Just "podman" -> do
      baseEnvironment <- getEnvironment
      return $ Just (("KIND_EXPERIMENTAL_PROVIDER", "podman") : baseEnvironment)
    Just x -> expectationFailure [i|Unexpected driver: #{x}|]
    Nothing -> return Nothing

  let driver = fromMaybe "docker" kindClusterDriver

  (bracket (startKindCluster opts clusterName kindConfigFile kindKubeConfigFile environmentToUse driver)
           (\_ -> do
               ps <- createProcessWithLogging ((proc kindBinary ["delete", "cluster", "--name", toString clusterName]) {
                                                  env = environmentToUse
                                                  })
               void $ waitForProcess ps
           ))
           (\kcc -> bracket_ (setUpKindCluster kcc environmentToUse driver)
                             (return ())
                             (action kcc)
           )

startKindCluster :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => KindClusterOptions -> Text -> FilePath -> FilePath -> Maybe [(String, String)] -> Text -> m KubernetesClusterContext
startKindCluster (KindClusterOptions {..}) clusterName kindConfigFile kindKubeConfigFile environmentToUse driver = do
  ps <- createProcessWithLogging ((proc "kind" ["create", "cluster", "-v", "1", "--name", toString clusterName
                                               , "--config", kindConfigFile
                                               , "--kubeconfig", kindKubeConfigFile]) {
                                     delegate_ctlc = True
                                     , env = environmentToUse
                                     })
  void $ waitForProcess ps

  whenM isInContainer $
    callCommandWithLogging [i|kind get kubeconfig --internal --name #{clusterName} > "#{kindKubeConfigFile}"|]

  oidcCache <- newTVarIO mempty
  (m, c) <- liftIO $ mkKubeClientConfig oidcCache $ KubeConfigFile kindKubeConfigFile

  pure $ KubernetesClusterContext {
    kubernetesClusterName = toText clusterName
    , kubernetesClusterKubeConfigPath = kindKubeConfigFile
    , kubernetesClusterNumNodes = kindClusterNumNodes
    , kubernetesClusterClientConfig = (m, c)
    , kubernetesClusterType = KubernetesClusterKind clusterName driver environmentToUse
    }
