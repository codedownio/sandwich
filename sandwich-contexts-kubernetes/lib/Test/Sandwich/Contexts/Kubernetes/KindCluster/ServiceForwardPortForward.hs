{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.ServiceForwardPortForward where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Network.URI
import Relude hiding (withFile)
import Safe
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.KubectlPortForward
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Environment
import UnliftIO.Process


withForwardKubernetesService' :: (
  MonadUnliftIO m, MonadCatch m, MonadLoggerIO m
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> FilePath -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService' (KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterKind {..}), ..}) kubectlBinary namespace service action = do
  baseEnv <- maybe getEnvironment return kubernetesClusterTypeKindClusterEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  portRaw <- (toString . T.strip . toText) <$> readCreateProcessWithLogging (
    (proc kubectlBinary [
      "get"
      , "service", toString service
      , "--namespace", toString namespace
      , [i|-o=jsonpath={.spec.ports[0].port}|]
      ]) { env = Just env }) ""

  port <- case readMay portRaw of
    Just p -> pure p
    Nothing -> expectationFailure [i|Failed to parse service port: #{portRaw}|]

  withKubectlPortForward' kubectlBinary kubernetesClusterKubeConfigPath namespace (const True) Nothing ("svc/" <> service) port $ \(KubectlPortForwardContext {..}) -> do
    action $ nullURI {
      uriScheme = "http:"
      , uriAuthority = Just (nullURIAuth {
                                uriRegName = "localhost"
                                , uriPort = ":" <> show kubectlPortForwardPort
                                })
      }

withForwardKubernetesService' _ _ _ _ _ = error "withForwardKubernetesService' must be called with a kind KubernetesClusterContext"
