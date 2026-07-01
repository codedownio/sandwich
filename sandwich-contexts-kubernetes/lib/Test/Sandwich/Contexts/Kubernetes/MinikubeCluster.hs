{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

Create and manage Kubernetes clusters via [Minikube](https://minikube.sigs.k8s.io).

-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster (
  -- * Introducing a cluster via Minikube
  introduceMinikubeClusterViaNix
  , introduceMinikubeClusterViaEnvironment
  , introduceMinikubeCluster'

  -- * Bracket-style functions
  , withMinikubeCluster
  , withMinikubeCluster'
  , withMinikubeCluster''

  -- * Image management
  -- | These are lower-level and Minikube-specific; prefer working with the functions in "Test.Sandwich.Contexts.Kubernetes.Images".
  , Images.clusterContainsImageMinikube
  , Images.getLoadedImagesMinikube
  , Images.loadImageMinikube

  -- * Re-exported cluster types
  , kubernetesCluster
  , KubernetesClusterContext (..)
  , HasKubernetesClusterContext

  -- * Types
  , MinikubeClusterOptions (..)
  , defaultMinikubeClusterOptions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Kubernetes.Client.Config
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import qualified Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images as Images
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.UUID
import Test.Sandwich.Contexts.Nix
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process


data MinikubeClusterOptions = MinikubeClusterOptions {
  minikubeClusterNumNodes :: Int
  , minikubeClusterExtraFlags :: [Text]
  , minikubeClusterNamePrefix :: Maybe Text
  , minikubeClusterDriver :: Maybe Text
  , minikubeClusterCpus :: Maybe Text
  , minikubeClusterMemory :: Maybe Text
  -- | Extra environment variables to set on every @minikube@ invocation for this cluster
  -- (start, delete, image loads, service forwards, @ssh@). These are merged over (and override)
  -- the ambient process environment. Use this to scope e.g. @MINIKUBE_HOME@ to a single cluster
  -- instead of mutating the global process environment.
  , minikubeClusterExtraEnv :: [(String, String)]
  }
defaultMinikubeClusterOptions :: MinikubeClusterOptions
defaultMinikubeClusterOptions = MinikubeClusterOptions {
  minikubeClusterNumNodes = 3
  , minikubeClusterExtraFlags = []
  , minikubeClusterNamePrefix = Nothing
  , minikubeClusterDriver = Nothing
  , minikubeClusterCpus = Nothing
  , minikubeClusterMemory = Nothing
  , minikubeClusterExtraEnv = []
  }

-- * Introduce

type MinikubeClusterContext context =
  LabelValue "kubernetesCluster" KubernetesClusterContext
  :> LabelValue "file-minikube" (EnvironmentFile "minikube")
  :> context

-- | Introduce a Minikube cluster, deriving the @minikube@ binary from the Nix context.
introduceMinikubeClusterViaNix :: (
  HasBaseContext context, MonadUnliftIO m, HasNixContext context
  )
  -- | Options
  => MinikubeClusterOptions
  -- | Child spec
  -> SpecFree (MinikubeClusterContext context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceMinikubeClusterViaNix minikubeClusterOptions spec =
  introduceBinaryViaNixPackage @"minikube" "minikube" $
    introduceWith "introduce minikube cluster" kubernetesCluster (void . withMinikubeCluster minikubeClusterOptions) spec

-- | Introduce a Minikube cluster, deriving the @minikube@ binary from the PATH.
introduceMinikubeClusterViaEnvironment :: (
  HasBaseContext context, MonadUnliftIO m
  )
  -- | Options
  => MinikubeClusterOptions
  -> SpecFree (MinikubeClusterContext context) m ()
  -> SpecFree context m ()
introduceMinikubeClusterViaEnvironment minikubeClusterOptions spec =
  introduceBinaryViaEnvironment @"minikube" $
    introduceWith "introduce minikube cluster" kubernetesCluster (void . withMinikubeCluster minikubeClusterOptions) spec

-- | Introduce a Minikube cluster, passing in the @minikube@ binary path.
introduceMinikubeCluster' :: (
  HasBaseContext context, MonadUnliftIO m
  )
  -- | Path to @minikube@ binary
  => FilePath
  -> MinikubeClusterOptions
  -> SpecFree (MinikubeClusterContext context) m ()
  -> SpecFree context m ()
introduceMinikubeCluster' minikubeBinary minikubeClusterOptions spec =
  introduceFile @"minikube" minikubeBinary $
    introduceWith "introduce minikube cluster" kubernetesCluster (void . withMinikubeCluster minikubeClusterOptions) $
      spec

-- * Implementation

-- | Bracket-style variant for introducing a Minikube cluster, using a @HasFile context "minikube"@ constraint.
withMinikubeCluster :: (
  HasBaseContextMonad context m, HasFile context "minikube"
  , MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  )
  -- | Options
  => MinikubeClusterOptions
  -> (KubernetesClusterContext -> m a)
  -> m a
withMinikubeCluster options action = do
  minikubeBinary <- askFile @"minikube"
  withMinikubeCluster' minikubeBinary options action

-- | Same as 'withMinikubeCluster', but allows you to pass the path to the @minikube@ binary.
withMinikubeCluster' :: (
  HasBaseContextMonad context m
  , MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  )
  -- | Path to @minikube@ binary
  => FilePath
  -> MinikubeClusterOptions
  -> (KubernetesClusterContext -> m a)
  -> m a
withMinikubeCluster' minikubeBinary options@(MinikubeClusterOptions {..}) action = do
  let prefix = fromMaybe "test-minikube-cluster" minikubeClusterNamePrefix
  clusterID <- makeUUID' 5
  let clusterName = [i|#{prefix}-#{clusterID}|]
  withMinikubeCluster'' clusterName minikubeBinary options action

-- | Same as 'withMinikubeCluster'', but allows you to pass the cluster name.
withMinikubeCluster'' :: (
  HasBaseContextMonad context m
  , MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  )
  -- | Cluster name
  => String
  -> FilePath
  -> MinikubeClusterOptions
  -> (KubernetesClusterContext -> m a)
  -> m a
withMinikubeCluster'' clusterName minikubeBinary options@(MinikubeClusterOptions {..}) action = do
  Just dir <- getCurrentFolder

  minikubeDir <- liftIO $ createTempDirectory dir "minikube"

  let minikubeKubeConfigFile = minikubeDir </> "minikube-config"
  writeFile minikubeKubeConfigFile ""

  let startLogFile = minikubeDir </> "minikube-start.log"
  let deleteLogFile = minikubeDir </> "minikube-delete.log"

  withFile startLogFile WriteMode $ \logH ->
    (bracket (timeAction "start Minikube cluster" $ startMinikubeCluster minikubeBinary logH clusterName minikubeKubeConfigFile options)
             (\_ -> timeAction "delete Minikube cluster" $ do
                 info [i|Deleting minikube cluster: #{clusterName}|]

                 -- For container-runtime drivers (docker/podman) minikube creates a volume per node.
                 -- Capture their names now, while the node containers still exist and carry minikube's
                 -- labels (see getMinikubeClusterVolumeNames), so we can mop up any volumes that
                 -- "minikube delete" leaves behind below. VM-based drivers (kvm2, etc.) have no such
                 -- volumes, so we skip them.
                 let containerRuntime = fromMaybe "docker" minikubeClusterDriver
                 let usingContainerDriver = containerRuntime `L.elem` ["docker", "podman"]
                 volumeNames <- if usingContainerDriver
                   then getMinikubeClusterVolumeNames containerRuntime clusterName
                   else pure []

                 let extraFlags = case "--rootless" `L.elem` minikubeClusterExtraFlags of
                       True -> ["--rootless"]
                       False -> []

                 deleteEnv <- addOrReplaceEnv minikubeClusterExtraEnv <$> getEnvironment
                 withFile deleteLogFile WriteMode $ \deleteH -> do
                   let deleteCp = (proc minikubeBinary (["delete"
                                                        , "--profile", clusterName
                                                        , "--logtostderr"
                                                        ] <> extraFlags)) {
                         delegate_ctlc = True
                         , create_group = True
                         , env = Just deleteEnv
                         , std_out = UseHandle deleteH
                         , std_err = UseHandle deleteH
                         }
                   withCreateProcess deleteCp $ \_ _ _ p ->
                     waitForProcess p >>= \case
                       ExitSuccess -> return ()
                       ExitFailure n -> warn [i|Minikube cluster delete failed with code #{n}.|]

                 -- "minikube delete" sometimes leaves behind the container-runtime volumes and the
                 -- per-profile state under MINIKUBE_HOME that it created. Remove these best-effort so
                 -- they don't accumulate as orphans across runs.
                 deleteClusterVolumes containerRuntime volumeNames
                 removeMinikubeProfileDirs clusterName minikubeClusterExtraEnv
             ))
             (\p -> timeAction "use Minikube cluster" $ do
                 waitForProcess p >>= \case
                   ExitSuccess -> return ()
                   ExitFailure n -> expectationFailure [i|Minikube cluster creation failed with code #{n}.|]

                 oidcCache <- newTVarIO mempty
                 (m, c) <- liftIO $ mkKubeClientConfig oidcCache $ KubeConfigFile minikubeKubeConfigFile

                 action $ KubernetesClusterContext {
                   kubernetesClusterName = toText clusterName
                   , kubernetesClusterKubeConfigPath = minikubeKubeConfigFile
                   , kubernetesClusterNumNodes = minikubeClusterNumNodes
                   , kubernetesClusterClientConfig = (m, c)
                   , kubernetesClusterType = KubernetesClusterMinikube {
                       kubernetesClusterTypeMinikubeBinary = minikubeBinary
                       , kubernetesClusterTypeMinikubeProfileName = toText clusterName
                       , kubernetesClusterTypeMinikubeFlags = minikubeClusterExtraFlags
                       , kubernetesClusterTypeMinikubeExtraEnvironment = minikubeClusterExtraEnv
                       }
                   }
             )

startMinikubeCluster :: (
  MonadLoggerIO m
  ) => FilePath -> Handle -> String -> String -> MinikubeClusterOptions -> m ProcessHandle
startMinikubeCluster minikubeBinary logH clusterName minikubeKubeConfigFile (MinikubeClusterOptions {..}) = do
  baseEnv <- getEnvironment
  let env = addOrReplaceEnv minikubeClusterExtraEnv (("KUBECONFIG", minikubeKubeConfigFile) : baseEnv)

  -- Note: this doesn't actually work! These options actually go to the docker daemon, not the "start" operation.
  -- It may not be possible to get a label on the Docker container in current minikube.
  -- let labelArgs = case dockerLabels of
  --       x | M.null x -> []
  --       xs -> "--docker-opt" : [[i|label=#{k}=#{v}|] | (k, v) <- M.toList xs]

  let driverAndResourceFlags = case minikubeClusterDriver of
        Nothing -> ["--driver=docker"
                   , [i|--memory=#{fromMaybe "16000mb" minikubeClusterMemory}|]
                   , [i|--cpus=#{fromMaybe "max" minikubeClusterCpus}|]
                   ]
        Just d -> [[i|--driver=#{d}|]
                  , [i|--memory=#{fromMaybe "16000mb" minikubeClusterMemory}|]
                  , [i|--cpus=#{fromMaybe "8" minikubeClusterCpus}|]
                  ]

  let args = ["start"
             , "--profile", clusterName
             , "--logtostderr"
             -- , "--addons=ingress"
             , "--extra-config=kubelet.streaming-connection-idle-timeout=5h"
             ]
             <> driverAndResourceFlags
             <> (fmap toString minikubeClusterExtraFlags)

  info [i|export KUBECONFIG='#{minikubeKubeConfigFile}'|]
  debug [i|Starting minikube with args: #{minikubeBinary} #{T.unwords $ fmap toText args}|]

  (_, _, _, p) <- createProcess (
    (proc minikubeBinary args) {
        delegate_ctlc = True
        , create_group = True
        , env = Just env
        , std_out = UseHandle logH
        , std_err = UseHandle logH
        })
  return p

-- | Authoritative source for a cluster's container-runtime volume names. Minikube tags every node
-- container of a profile with the label @mode.minikube.sigs.k8s.io=<profile>@, and for the docker
-- and podman drivers each node's persistent volume shares its container's name. So we ask the
-- runtime (@docker@ or @podman@) for the containers rather than reconstructing minikube's @-mNN@
-- node-naming scheme. Must be called before "minikube delete", while the containers still exist.
getMinikubeClusterVolumeNames :: (MonadLoggerIO m, MonadUnliftIO m) => Text -> String -> m [String]
getMinikubeClusterVolumeNames containerRuntime clusterName =
  handleAny (\e -> [] <$ warn [i|Couldn't list #{containerRuntime} containers for cluster #{clusterName}: #{e}|]) $
    readCreateProcessWithExitCode (proc (toString containerRuntime) [
        "ps", "-a"
        , "--filter", [i|label=mode.minikube.sigs.k8s.io=#{clusterName}|]
        , "--format", "{{.Names}}"
        ]) "" >>= \case
      (ExitSuccess, out, _) -> pure [toString l | l <- T.lines (toText out), not (T.null (T.strip l))]
      (ExitFailure _, _, _) -> pure []

-- | Force-remove the given container-runtime volumes ("--force" ignores any that are already gone,
-- and is supported by both @docker volume rm@ and @podman volume rm@). Best-effort.
deleteClusterVolumes :: (MonadLoggerIO m, MonadUnliftIO m) => Text -> [String] -> m ()
deleteClusterVolumes _ [] = return ()
deleteClusterVolumes containerRuntime volumes = handleAny logErr $ do
  debug [i|Removing leftover #{containerRuntime} volumes: #{volumes}|]
  void $ readCreateProcessWithExitCode (proc (toString containerRuntime) (["volume", "rm", "--force"] <> volumes)) ""
  where
    logErr :: MonadLoggerIO m => SomeException -> m ()
    logErr e = warn [i|Error removing #{containerRuntime} volumes #{volumes}: #{e}|]

-- | Remove the per-profile state that "minikube delete" should clean up but sometimes leaves behind:
-- @<minikube-home>/profiles/<profile>@ and @.../machines/<profile>@. The rest of MINIKUBE_HOME (the
-- multi-gigabyte image caches, certs, addons) is shared across clusters, so we leave it untouched.
removeMinikubeProfileDirs :: (MonadLoggerIO m, MonadUnliftIO m) => String -> [(String, String)] -> m ()
removeMinikubeProfileDirs clusterName extraEnv = handleAny logErr $ do
  miniPath <- getMinikubeHome extraEnv
  forM_ ["profiles", "machines"] $ \sub -> do
    let dir = miniPath </> sub </> clusterName
    whenM (doesPathExist dir) $ do
      debug [i|Removing leftover minikube state dir: #{dir}|]
      removePathForcibly dir
  where
    logErr :: MonadLoggerIO m => SomeException -> m ()
    logErr e = warn [i|Error removing minikube state dirs for #{clusterName}: #{e}|]

-- | Resolve MINIKUBE_HOME the same way minikube does (see localpath.MiniPath): default to
-- @$HOME/.minikube@, honor a @MINIKUBE_HOME@ that already ends in @.minikube@, else append it.
-- A @MINIKUBE_HOME@ in the passed cluster-scoped env takes precedence over the ambient process env,
-- so profile cleanup targets the same home the cluster was actually started with.
getMinikubeHome :: MonadIO m => [(String, String)] -> m FilePath
getMinikubeHome extraEnv = resolve (L.lookup "MINIKUBE_HOME" extraEnv) >>= \case
  Just p -> return p
  Nothing -> UnliftIO.Environment.lookupEnv "MINIKUBE_HOME" >>= resolve >>= \case
    Just p -> return p
    Nothing -> (</> ".minikube") <$> getHomeDirectory
  where
    resolve :: MonadIO m => Maybe String -> m (Maybe FilePath)
    resolve = \case
      Just p | p /= "" -> return $ Just $ if takeFileName p == ".minikube" then p else p </> ".minikube"
      _ -> return Nothing

-- Debugging (in case of certificate issues such as https://github.com/channable/vaultenv/issues/99)
-- import Kubernetes.Client.Auth.OIDC
-- oidcCache :: OIDCCache <- Relude.newTVarIO mempty
-- (m, c) <- mkKubeClientConfig oidcCache $ KubeConfigFile "/tmp/test-minikube-cluster-config-e695417a5bf81acf/minikube-kube-config"
-- import Kubernetes.OpenAPI.Core
-- import Kubernetes.OpenAPI.API.AppsV1 as Kubernetes
-- import Kubernetes.OpenAPI.API.BatchV1 as Kubernetes
-- import Kubernetes.OpenAPI.API.CoreV1 as Kubernetes
-- import Kubernetes.OpenAPI.Core as Kubernetes
-- import Kubernetes.OpenAPI.MimeTypes
-- import Kubernetes.OpenAPI.Model as Kubernetes
-- import Kubernetes.OpenAPI.Client as Kubernetes
-- MimeResult parsedResult _httpResponse <- liftIO (dispatchMime m c (listNamespacedPod (Accept MimeJSON) (Namespace "default")))
