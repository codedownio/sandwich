{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinioOperator (
  introduceMinioOperator
  , introduceMinioOperator'

  -- * Bracket-style variants
  , withMinioOperator
  , withMinioOperator'

  -- * Types
  , minioOperator
  , MinioOperatorContext(..)
  , MinioOperatorOptions(..)
  , defaultMinioOperatorOptions
  , HasMinioOperatorContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson (FromJSON)
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Yaml as Yaml
import Kubernetes.OpenAPI.Model as Kubernetes
import Relude
import Safe
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.FindImages
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Exception
import UnliftIO.Process


data MinioOperatorOptions = MinioOperatorOptions {
  minioOperatorPreloadImages :: Bool
  }
defaultMinioOperatorOptions :: MinioOperatorOptions
defaultMinioOperatorOptions = MinioOperatorOptions {
  minioOperatorPreloadImages = True
  }

-- | Install the MinIO Kubernetes plugin onto a Kubernetes cluster.
-- See the docs [here](https://min.io/docs/minio/kubernetes/upstream/reference/kubectl-minio-plugin.html).
introduceMinioOperator :: (
  MonadUnliftIO m, HasBaseContext context, HasKubernetesClusterContext context, HasFile context "kubectl"
  ) => MinioOperatorOptions -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator options = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator options kcc action

-- | Same as 'introduceMinioOperator', but allows you to pass in the "kubectl" binary path.
introduceMinioOperator' :: (
  MonadUnliftIO m, MonadFail m, HasKubernetesClusterContext context, HasBaseContext context
  ) => MinioOperatorOptions -> FilePath -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m () -> SpecFree context m ()
introduceMinioOperator' options kubectlBinary = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator' options kubectlBinary kcc action

-- | Bracket-style variant of 'introduceMinioOperator'.
withMinioOperator :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  , HasBaseContextMonad context m, HasFile context "kubectl"
  ) => MinioOperatorOptions -> KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator options kcc action = do
  kubectlBinary <- askFile @"kubectl"
  withMinioOperator' options kubectlBinary kcc action

-- | Same as 'withMinioOperator', but allows you to pass in the "kubectl" binary path.
withMinioOperator' :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadFail m
  , HasBaseContextMonad context m
  ) => MinioOperatorOptions -> FilePath -> KubernetesClusterContext -> (MinioOperatorContext -> m a) -> m a
withMinioOperator' (MinioOperatorOptions {..}) kubectlBinary kcc action = do
  (_, env) <- runWithKubectl' kcc kubectlBinary

  allYaml <- readCreateProcessWithLogging ((proc kubectlBinary ["kustomize", "github.com/minio/operator?ref=v6.0.1"]) { env = Just env }) ""

  when minioOperatorPreloadImages $ do
    let images = findAllImages (toText allYaml)

    forM_ images $ \image ->
      loadImageIfNecessary' kcc (ImageLoadSpecDocker image IfNotPresent)

  let create = createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) allYaml
                 >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  let namespaceToDestroy = fromMaybe "minio-operator" (findNamespace (toText allYaml))
  info [i|Detected MinIO operator namespace: #{namespaceToDestroy}|]

  let destroy = do
        -- I think this is a robust way to delete everything?
        -- Just doing "delete -f -" produces errors, seemingly because the minio-operator Namespace
        -- gets deleted first and then subsequent deletes encounter missing objects.
        -- If this doesn't work, we can fall back to just deleting the namespace below.
        -- But I think this will be better because it should pick up CRDs?
        createProcessWithLoggingAndStdin ((proc kubectlBinary ["delete", "-f", "-"
                                                              , "--ignore-not-found", "--wait=false", "--all=true"
                                                              ]) {
                                             env = Just env, delegate_ctlc = True }) allYaml
          >>= waitForProcess >>= (`shouldBe` ExitSuccess)

        -- createProcessWithLogging ((proc kubectlBinary ["delete", "namespace", toString namespaceToDestroy, "-f"]) {
        --                              env = Just env, delegate_ctlc = True
        --                              })
        --   >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  bracket_ create destroy (action MinioOperatorContext)

-- | Find the first "Namespace" resource in some multi-document YAML and extract its name
findNamespace :: Text -> Maybe Text
findNamespace = headMay . mapMaybe findNamespace' . T.splitOn "---\n"
  where
    findNamespace' :: Text -> Maybe Text
    findNamespace' (decode -> Right (V1Namespace {v1NamespaceKind=(Just "Namespace"), v1NamespaceMetadata=(Just meta)})) = v1ObjectMetaName meta
    findNamespace' _ = Nothing

    decode :: FromJSON a => Text -> Either Yaml.ParseException a
    decode = Yaml.decodeEither' . encodeUtf8
