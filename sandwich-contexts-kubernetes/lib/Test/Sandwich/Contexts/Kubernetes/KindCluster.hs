{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster (
  introduceKindCluster

  -- * Bracket-style versions
  , withKindCluster
  , withKindCluster'

  -- * Loading images
  , withLoadImages
  , withLoadImages'

  -- * Types
  , KindClusterOptions (..)
  , KubernetesClusterContext (..)
  , defaultKindClusterOptions
  , kubernetesCluster
  , HasKubernetesClusterContext
  ) where

import Control.Exception.Lifted (bracket, bracket_)
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Kubernetes.Client.Config
import Relude
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Config
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Images
import Test.Sandwich.Contexts.Kubernetes.KindCluster.Setup
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Container (isInContainer)
import Test.Sandwich.Contexts.Kubernetes.Util.UUID
import UnliftIO.Environment
import UnliftIO.Process


-- Note: when using kind with podman as a driver, it's possible to run into a low PID limit
-- which isn't enough for all the processes in a Kubernetes cluster.
-- I debugged this and found a kind patch to fix it, described here:
-- https://github.com/kubernetes-sigs/kind/issues/3451#issuecomment-1855701939
--
-- You can also fix this at the podman level, with the following in `containers.conf`:
-- [containers]
-- pids_limit = 0


data KindClusterOptions = KindClusterOptions {
  kindClusterNumNodes :: Int
  , kindClusterExtraFlags :: [Text]
  , kindClusterContainerLabels :: Map Text Text
  , kindClusterBinaryCache :: Maybe FilePath
  , kindClusterNamePrefix :: Maybe Text
  , kindClusterDriver :: Maybe Text
  , kindClusterCpus :: Maybe Text
  , kindClusterMemory :: Maybe Text
  }
defaultKindClusterOptions :: KindClusterOptions
defaultKindClusterOptions = KindClusterOptions {
  kindClusterNumNodes = 3
  , kindClusterExtraFlags = []
  , kindClusterContainerLabels = mempty
  , kindClusterBinaryCache = Nothing
  , kindClusterNamePrefix = Nothing
  , kindClusterDriver = Nothing
  , kindClusterCpus = Nothing
  , kindClusterMemory = Nothing
  }

-- * Introduce

-- | Introduce a Kubernetes cluster using [kind](https://kind.sigs.k8s.io/).
introduceKindCluster :: (
  MonadUnliftIO m, MonadBaseControl IO m, MonadMask m
  )
  -- | Options
  => KindClusterOptions
  -- | Child spec
  -> SpecFree (LabelValue "kubernetesCluster" KubernetesClusterContext :> context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceKindCluster opts@(KindClusterOptions {}) = introduceWith "introduce kind cluster" kubernetesCluster $ \action ->
  void $ withKindCluster opts action

-- * Implementation

-- | Bracket-style variant of 'introduceKindCluster'.
withKindCluster :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadBaseControl IO m, MonadMask m
  )
  -- | Options
  => KindClusterOptions
  -> (KubernetesClusterContext -> m a)
  -> m a
withKindCluster opts@(KindClusterOptions {..}) action = do
  let prefix = fromMaybe "test-kind-cluster" kindClusterNamePrefix
  clusterID <- makeUUID' 5
  let clusterName = [i|#{prefix}-#{clusterID}|]
  withKindCluster' opts clusterName action

-- | Same as 'withKindCluster', but allows you to control the cluster name.
withKindCluster' :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadBaseControl IO m, MonadMask m
  ) => KindClusterOptions -> Text -> (KubernetesClusterContext -> m a) -> m a
withKindCluster' opts@(KindClusterOptions {..}) clusterName action = do
  kc <- isInContainer >>= \case
    False -> return $ kindConfig kindClusterContainerLabels Nothing kindClusterNumNodes kindClusterBinaryCache
    True -> return $ kindConfig kindClusterContainerLabels Nothing kindClusterNumNodes kindClusterBinaryCache

  withSystemTempDirectory "test-kind-cluster-config" $ \tempDir -> do
    let kindConfigFile = tempDir </> "kind-config"
    writeFile kindConfigFile (toString kc)
    info [i|kindConfigFile: #{kindConfigFile}|]

    let kindKubeConfigFile = tempDir </> "kind-kube-config"
    writeFile kindKubeConfigFile ""

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
                 ps <- createProcessWithLogging ((proc "kind" ["delete", "cluster", "--name", toString clusterName]) {
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
