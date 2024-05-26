{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster (
  -- * Introducing a cluster via Minikube
  introduceMinikubeClusterViaNix
  , introduceMinikubeClusterViaEnvironment
  , introduceMinikubeCluster'

  -- * Bracket-style functions
  , withMinikubeCluster
  , withMinikubeCluster'

  -- * For loading images onto the cluster
  , introduceImages
  , withLoadImages
  , withLoadImages'

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
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Images
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

introduceMinikubeClusterViaNix :: (
  HasBaseContext context, MonadUnliftIO m, HasNixContext context
  )
  -- | Options
  => MinikubeClusterOptions
  -- | Child spec
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> LabelValue "file-minikube" (EnvironmentFile "minikube") :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceMinikubeClusterViaNix minikubeClusterOptions spec =
  introduceBinaryViaNixPackage @"minikube" "minikube" $
    introduceWith "introduce minikube cluster" kubernetesCluster (void . withMinikubeCluster minikubeClusterOptions) spec

introduceMinikubeClusterViaEnvironment :: (
  HasBaseContext context, MonadUnliftIO m
  )
  -- | Options
  => MinikubeClusterOptions
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> LabelValue "file-minikube" (EnvironmentFile "minikube") :> context) m ()
  -> SpecFree context m ()
introduceMinikubeClusterViaEnvironment minikubeClusterOptions spec =
  introduceBinaryViaEnvironment @"minikube" $
    introduceWith "introduce minikube cluster" kubernetesCluster (void . withMinikubeCluster minikubeClusterOptions) spec

introduceMinikubeCluster' :: (
  HasBaseContext context, MonadUnliftIO m
  )
  -- | Path to minikube binary
  => FilePath
  -> MinikubeClusterOptions
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> LabelValue "file-minikube" (EnvironmentFile "minikube") :> context) m ()
  -> SpecFree context m ()
introduceMinikubeCluster' minikubeBinary minikubeClusterOptions spec =
  introduceFile @"minikube" minikubeBinary $
    introduceWith "introduce minikube cluster" kubernetesCluster (void . withMinikubeCluster minikubeClusterOptions) $
      spec

-- * Implementation

-- | Bracket-style variant for introducing a Minikube cluster, using a 'HasFile context "minikube"' constraint.
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

-- | Same as 'withMinikubeCluster', but allows you to pass the path to the Minikube binary.
withMinikubeCluster' :: (
  HasBaseContextMonad context m
  , MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  ) => FilePath -> MinikubeClusterOptions -> (KubernetesClusterContext -> m a) -> m a
withMinikubeCluster' minikubeBinary options@(MinikubeClusterOptions {..}) action = do
  let prefix = fromMaybe "test-minikube-cluster" minikubeClusterNamePrefix
  clusterID <- makeUUID' 5
  let clusterName = [i|#{prefix}-#{clusterID}|]
  withNewMinikubeCluster minikubeBinary clusterName options action

withNewMinikubeCluster :: (
  HasBaseContextMonad context m
  , MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  ) => FilePath -> String -> MinikubeClusterOptions -> (KubernetesClusterContext -> m a) -> m a
withNewMinikubeCluster minikubeBinary clusterName options@(MinikubeClusterOptions {..}) action = do
  Just dir <- getCurrentFolder

  minikubeDir <- liftIO $ createTempDirectory dir "minikube"

  let minikubeKubeConfigFile = minikubeDir </> "minikube-config"
  writeFile minikubeKubeConfigFile ""

  let startLogFile = minikubeDir </> "minikube-start.log"
  let deleteLogFile = minikubeDir </> "minikube-delete.log"

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
                       minikubeBinary = minikubeBinary
                       , minikubeProfileName = toText clusterName
                       , minikubeFlags = minikubeClusterExtraFlags
                       }
                   }
             )

startMinikubeCluster :: (
  MonadLoggerIO m, MonadReader context m
  ) => FilePath -> Handle -> String -> String -> MinikubeClusterOptions -> m ProcessHandle
startMinikubeCluster minikubeBinary logH clusterName minikubeKubeConfigFile (MinikubeClusterOptions {..}) = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", minikubeKubeConfigFile) : baseEnv)

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
