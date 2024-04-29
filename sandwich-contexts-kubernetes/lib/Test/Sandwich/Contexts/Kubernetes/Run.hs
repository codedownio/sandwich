{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sandwich.Contexts.Kubernetes.Run where

import Test.Sandwich.Contexts.Kubernetes.Types
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String.Interpolate
import Data.Text as T
import Kubernetes.OpenAPI.Client as Kubernetes
import Kubernetes.OpenAPI.Core as Kubernetes
import Kubernetes.OpenAPI.MimeTypes
import Network.HTTP.Client
import Relude
import Test.Sandwich
import UnliftIO.Exception


type Constraints context m = (MonadIO m, MonadThrow m, MonadUnliftIO m, MonadLogger m, MonadMask m, MonadReader context m)

instance Exception MimeError

-- * Run Exception

k8sRunException :: (Produces req accept, MimeUnrender accept res, MimeType contentType, Constraints context m, HasKubernetesClusterContext context)
  => KubernetesRequest req contentType res accept -> m res
k8sRunException req = do
  (manager, clientConfig) <- kubernetesClusterClientConfig <$> getContext kubernetesCluster
  k8sRunException' manager clientConfig req

k8sRunException' :: (MimeUnrender accept res, MimeType contentType, Produces req accept, Constraints context m)
  => Manager -> KubernetesClientConfig -> KubernetesRequest req contentType res accept -> m res
k8sRunException' manager clientConfig req = k8sRunEither'' manager clientConfig req >>= \case
  Left err -> throwIO err
  Right x -> return x

-- * Run Either

k8sRunEither :: (Produces req accept, MimeUnrender accept res, MimeType contentType, Constraints context m, HasKubernetesClusterContext context)
  => KubernetesRequest req contentType res accept -> m (Either Text res)
k8sRunEither req = do
  (manager, clientConfig) <- kubernetesClusterClientConfig <$> getContext kubernetesCluster
  k8sRunEither' manager clientConfig req

k8sRunEither' :: (Produces req accept, MimeUnrender accept res, MimeType contentType, Constraints context m)
  => Manager -> KubernetesClientConfig -> KubernetesRequest req contentType res accept -> m (Either Text res)
k8sRunEither' manager clientConfig req = first show <$> k8sRunEither'' manager clientConfig req

k8sRunEither'' :: (Produces req accept, MimeUnrender accept res, MimeType contentType, Constraints context m)
  => Manager -> KubernetesClientConfig -> KubernetesRequest req contentType res accept -> m (Either MimeError res)
k8sRunEither'' k8sManager k8sClientConfig req = do
  MimeResult parsedResult _httpResponse <- liftIO (dispatchMime k8sManager k8sClientConfig req)

  let successMessage = case parsedResult of
        Left err -> "FAIL: " <> show err
        _ -> "SUCCESS" :: Text

  debug [i|Kubernetes request: #{rMethod req} to #{BL.intercalate "/" $ rUrlPath req} = #{successMessage}|]

  return parsedResult
