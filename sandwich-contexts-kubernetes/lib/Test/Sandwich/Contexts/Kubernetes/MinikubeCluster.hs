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
  }
defaultMinikubeClusterOptions :: MinikubeClusterOptions
defaultMinikubeClusterOptions = MinikubeClusterOptions {
  minikubeClusterNumNodes = 3
  , minikubeClusterExtraFlags = []
  , minikubeClusterNamePrefix = Nothing
  , minikubeClusterDriver = Nothing
  , minikubeClusterCpus = Nothing
  , minikubeClusterMemory = Nothing
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

  info [i|withMinikubeCluster'' 1|]

  minikubeDir <- liftIO $ createTempDirectory dir "minikube"

  let minikubeKubeConfigFile = minikubeDir </> "minikube-config"
  writeFile minikubeKubeConfigFile ""

  let startLogFile = minikubeDir </> "minikube-start.log"
  let deleteLogFile = minikubeDir </> "minikube-delete.log"

  info [i|withMinikubeCluster'' 2|]

  withFile startLogFile WriteMode $ \logH ->
    (bracket (startMinikubeCluster minikubeBinary logH clusterName minikubeKubeConfigFile options)
             (\_ -> do
                 info [i|Deleting minikube cluster: #{clusterName}|]

                 let extraFlags = case "--rootless" `L.elem` minikubeClusterExtraFlags of
                       True -> ["--rootless"]
                       False -> []

                 withFile deleteLogFile WriteMode $ \deleteH -> do
                   let deleteCp = (proc minikubeBinary (["delete"
                                                        , "--profile", clusterName
                                                        , "--logtostderr"
                                                        ] <> extraFlags)) {
                         delegate_ctlc = True
                         , create_group = True
                         , std_out = UseHandle deleteH
                         , std_err = UseHandle deleteH
                         }
                   withCreateProcess deleteCp $ \_ _ _ p ->
                     waitForProcess p >>= \case
                       ExitSuccess -> return ()
                       ExitFailure n -> warn [i|Minikube cluster delete failed with code #{n}.|]
             ))
             (\p -> do
                 info [i|withMinikubeCluster'' 3|]

                 waitForProcess p >>= \case
                   ExitSuccess -> return ()
                   ExitFailure n -> expectationFailure [i|Minikube cluster creation failed with code #{n}.|]

                 info [i|withMinikubeCluster'' 4|]

                 oidcCache <- newTVarIO mempty
                 (m, c) <- liftIO $ mkKubeClientConfig oidcCache $ KubeConfigFile minikubeKubeConfigFile

                 info [i|withMinikubeCluster'' 5|]

                 action $ KubernetesClusterContext {
                   kubernetesClusterName = toText clusterName
                   , kubernetesClusterKubeConfigPath = minikubeKubeConfigFile
                   , kubernetesClusterNumNodes = minikubeClusterNumNodes
                   , kubernetesClusterClientConfig = (m, c)
                   , kubernetesClusterType = KubernetesClusterMinikube {
                       kubernetesClusterTypeMinikubeBinary = minikubeBinary
                       , kubernetesClusterTypeMinikubeProfileName = toText clusterName
                       , kubernetesClusterTypeMinikubeFlags = minikubeClusterExtraFlags
                       }
                   }
             )

startMinikubeCluster :: (
  MonadLoggerIO m
  ) => FilePath -> Handle -> String -> String -> MinikubeClusterOptions -> m ProcessHandle
startMinikubeCluster minikubeBinary logH clusterName minikubeKubeConfigFile (MinikubeClusterOptions {..}) = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", minikubeKubeConfigFile) : baseEnv)

  info [i|startMinikubeCluster 1|]

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

  info [i|startMinikubeCluster 2|]

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
