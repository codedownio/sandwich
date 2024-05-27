{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.Waits where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import Kubernetes.OpenAPI.API.CoreV1 as Kubernetes
import Kubernetes.OpenAPI.Core as Kubernetes
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model as Kubernetes
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Run
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Waits
import UnliftIO.Process


waitForServiceEndpointsToExist :: (
  MonadUnliftIO m, MonadLogger m, MonadMask m
  , MonadReader context m, HasKubernetesClusterContext context
  ) => Text -> Text -> Double -> m ()
waitForServiceEndpointsToExist namespace serviceName timeInSeconds = do
  waitUntil timeInSeconds $ do
    endpoints <- listEndpoints namespace mempty
    case Relude.filter v1EndpointsSatisfies endpoints of
      [] -> expectationFailure [i|No endpoints were satisfactory|]
      (x:_) -> do
        debug [i|(#{namespace}) Got satisfactory endpoint for #{serviceName}: #{x}|]
        return ()

  where
    v1EndpointsSatisfies (V1Endpoints {v1EndpointsMetadata=(Just (V1ObjectMeta {v1ObjectMetaName=(Just name)})), v1EndpointsSubsets})
      | name == serviceName = L.all isSatisfactoryV1EndpointSubset (fromMaybe [] v1EndpointsSubsets)
    v1EndpointsSatisfies _ = False

    isSatisfactoryV1EndpointSubset (V1EndpointSubset { v1EndpointSubsetAddresses=(Just addrs), v1EndpointSubsetNotReadyAddresses=(fromMaybe [] -> notReadyAddrs) }) =
      not (L.null addrs)
      && L.null notReadyAddrs
    isSatisfactoryV1EndpointSubset _ = False


listEndpoints :: (
  MonadUnliftIO m, MonadLogger m, MonadMask m
  , MonadReader context m, HasKubernetesClusterContext context
  ) => Text -> Map Text Text -> m [V1Endpoints]
listEndpoints namespace labels =
  (v1EndpointsListItems <$>) $ k8sRunException (
      (listNamespacedEndpoints (Accept MimeJSON) (Namespace namespace))
      -&- (LabelSelector (T.intercalate "," [k <> "=" <> v | (k, v) <- M.toList labels]))
    )

waitForPodsToExist :: (
  MonadUnliftIO m, MonadLogger m, MonadMask m
  , MonadReader context m, HasKubernetesClusterContext context
  ) => Text -> Map Text Text -> Double -> Maybe Int -> m ()
waitForPodsToExist namespace labels timeInSeconds maybeDesiredCount = do
  waitUntil timeInSeconds $ do
    pods <- listPods namespace labels
    case maybeDesiredCount of
      Nothing -> when (L.null pods) $ expectationFailure [i|Found no pods.|]
      Just n -> when (L.length pods /= n) $ expectationFailure [i|Expected #{n} pods, but found #{L.length pods}|]

listPods :: (
  MonadUnliftIO m, MonadLogger m, MonadMask m
  , MonadReader context m, HasKubernetesClusterContext context
  ) => Text -> Map Text Text -> m [V1Pod]
listPods namespace labels =
  (v1PodListItems <$>) $ k8sRunException (
      (listNamespacedPod (Accept MimeJSON) (Namespace namespace))
      -&- (LabelSelector (T.intercalate "," [k <> "=" <> v | (k, v) <- M.toList labels]))
    )

waitForPodsToBeReady :: (
  MonadUnliftIO m, MonadLogger m
  , MonadReader context m, HasKubernetesClusterContext context, HasFile context "kubectl"
  ) => Text -> Map Text Text -> Double -> m ()
waitForPodsToBeReady namespace labels timeInSeconds = do
  kubectlBinary <- askFile @"kubectl"
  kubeConfigFile <- kubernetesClusterKubeConfigPath <$> getContext kubernetesCluster

  let labelArgs = [[i|-l #{k}=#{v}|] | (k, v) <- M.toList labels]
  p <- createProcessWithLogging (proc kubectlBinary (
                                  ["wait", "pods"
                                  , "--kubeconfig", kubeConfigFile
                                  , "-n", toString namespace
                                  ]
                                  <> labelArgs
                                  <> [
                                    "--for", "condition=Ready"
                                    , "--timeout=" <> show timeInSeconds <> "s"
                                    ]
                                ))
  waitForProcess p >>= \case
    ExitSuccess -> return ()
    ExitFailure n -> expectationFailure [i|Failed to wait for pods to exist (code #{n})|]
