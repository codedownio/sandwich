{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.ServiceForwardPortForward where

import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.KubectlPortForward
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Network.URI
import Relude hiding (withFile)
import Safe
import Test.Sandwich
import UnliftIO.Environment
import UnliftIO.Process


withForwardKubernetesService' :: (
  MonadUnliftIO m, MonadCatch m, MonadBaseControl IO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService' (KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterKind {..}), ..}) namespace service action = do
  baseEnv <- maybe getEnvironment return kindClusterEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  portRaw <- (toString . T.strip . toText) <$> readCreateProcessWithLogging (
    (proc "kubectl" [
      "get"
      , "service", toString service
      , "--namespace", toString namespace
      , [i|-o=jsonpath={.spec.ports[0].port}|]
      ]) { env = Just env }) ""

  port <- case readMay portRaw of
    Just p -> pure p
    Nothing -> expectationFailure [i|Failed to parse service port: #{portRaw}|]

  withKubectlPortForward kubernetesClusterKubeConfigPath namespace ("svc/" <> service) port $ \(KubectlPortForwardContext {..}) -> do
    action $ nullURI {
      uriScheme = "http:"
      , uriAuthority = Just (nullURIAuth {
                                uriRegName = "localhost"
                                , uriPort = ":" <> show kubectlPortForwardPort
                                })
      }

withForwardKubernetesService' _ _ _ _ = error "withForwardKubernetesService' must be called with a kind KubernetesClusterContext"
