{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

Install the [MinIO Kubernetes operator](https://min.io/docs/minio/kubernetes/upstream/operations/installation.html) onto a Kubernetes cluster.

This is necessary if you want to use the "Test.Sandwich.Contexts.Kubernetes.MinioS3Server" module to create actual S3 servers.

-}

module Test.Sandwich.Contexts.Kubernetes.MinioOperator (
  introduceMinioOperator
  , introduceMinioOperator'
  , introduceMinioOperator''

  -- * Bracket-style variants
  , withMinioOperator
  , withMinioOperator'

  -- * Misc
  , minioRepoDerivation

  -- * Types
  , minioOperator
  , MinioOperatorContext(..)

  , MinioOperatorOptions(..)
  , defaultMinioOperatorOptions

  , MinioOperatorYamlSource(..)

  , HasMinioOperatorContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
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
import Test.Sandwich.Contexts.Nix
import UnliftIO.Exception
import UnliftIO.Process


data MinioOperatorContext = MinioOperatorContext
  deriving (Show)

minioOperator :: Label "minioOperator" MinioOperatorContext
minioOperator = Label
type HasMinioOperatorContext context = HasLabel context "minioOperator" MinioOperatorContext

data MinioOperatorOptions = MinioOperatorOptions {
  minioOperatorPreloadImages :: Bool
  , minioOperatorYamlSource :: MinioOperatorYamlSource
  }
defaultMinioOperatorOptions :: MinioOperatorOptions
defaultMinioOperatorOptions = MinioOperatorOptions {
  minioOperatorPreloadImages = True
  , minioOperatorYamlSource = MinioOperatorYamlSourceGitHub "github.com/minio/operator?ref=v6.0.1"
  }

data MinioOperatorYamlSource =
  MinioOperatorYamlSourceGitHub String
  | MinioOperatorYamlSourceNixDerivation Text
  deriving (Show, Eq)

-- | Sample derivation for fetching the MinIO repo at version 6.0.4.
minioRepoDerivation :: Text
minioRepoDerivation =
  [__i|{ fetchFromGitHub }:

       fetchFromGitHub {
         owner = "minio";
         repo = "operator";
         rev = "c5b838c475609921935bd4f335fdbc4b6846be14";
         sha256 = "sha256-pWxBqfSxpTWylmR+Bz8+4gGlGwAiLY/3RFNU5mcnOPE=";
       }
      |]

-- | Install the [MinIO Kubernetes operator](https://min.io/docs/minio/kubernetes/upstream/operations/installation.html) onto a Kubernetes cluster.
introduceMinioOperator :: (
  Typeable context, KubectlBasicWithoutReader context m
  )
  -- | Options
  => MinioOperatorOptions
  -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m ()
  -> SpecFree context m ()
introduceMinioOperator options = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator kcc options action

-- | Same as 'introduceMinioOperator', but allows you to pass in the @kubectl@ binary path.
introduceMinioOperator' :: (
  HasCallStack, MonadFail m, MonadUnliftIO m
  , Typeable context, HasBaseContext context, HasKubernetesClusterContext context
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -- | Options
  -> MinioOperatorOptions
  -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m ()
  -> SpecFree context m ()
introduceMinioOperator' kubectlBinary options = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator' kubectlBinary kcc options action

-- | Same as 'introduceMinioOperator'', but allows you to pass in the options individually.
introduceMinioOperator'' :: (
  HasCallStack, MonadFail m, MonadUnliftIO m
  , HasBaseContext context, HasKubernetesClusterContext context
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -- | Whether to preload images
  -> Bool
  -- | Operator YAML
  -> Text
  -> SpecFree (LabelValue "minioOperator" MinioOperatorContext :> context) m ()
  -> SpecFree context m ()
introduceMinioOperator'' kubectlBinary preloadImages allYaml = introduceWith "introduce MinIO operator" minioOperator $ \action -> do
  kcc <- getContext kubernetesCluster
  void $ withMinioOperator'' kubectlBinary kcc preloadImages allYaml action

-- | Bracket-style variant of 'introduceMinioOperator'.
withMinioOperator :: (
  HasCallStack, MonadFail m, Typeable context, KubectlBasic context m
  )
  -- | Options
  => KubernetesClusterContext
  -> MinioOperatorOptions
  -> (MinioOperatorContext -> m a)
  -> m a
withMinioOperator kcc options action = do
  kubectlBinary <- askFile @"kubectl"
  withMinioOperator' kubectlBinary kcc options action

-- | Same as 'withMinioOperator', but allows you to pass in the @kubectl@ binary path.
withMinioOperator' :: (
  HasCallStack, MonadFail m, Typeable context, KubernetesBasic context m
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -> KubernetesClusterContext
  -- | Options
  -> MinioOperatorOptions
  -> (MinioOperatorContext -> m a)
  -> m a
withMinioOperator' kubectlBinary kcc opts action = do
  allYaml <- case minioOperatorYamlSource opts of
    MinioOperatorYamlSourceGitHub url -> readCreateProcessWithLogging (proc kubectlBinary ["kustomize", url]) ""
    MinioOperatorYamlSourceNixDerivation derivation ->
      getContextMaybe nixContext >>= \case
        Nothing -> expectationFailure [i|Couldn't find a Nix context to use with MinioOperatorYamlSourceNixDerivation|]
        Just nc -> do
          minioRepo <- buildNixCallPackageDerivation' nc derivation
          readCreateProcessWithLogging (proc kubectlBinary ["kustomize", minioRepo]) ""

  withMinioOperator'' kubectlBinary kcc (minioOperatorPreloadImages opts) (toText allYaml) action

-- | Same as 'withMinioOperator'', but allows you to pass in the options individually.
--
-- If you want to generate this YAML yourself, just run a command like
-- @kubectl kustomize "github.com/minio/operator?ref=v6.0.1"@
withMinioOperator'' :: (
  HasCallStack, MonadFail m, KubernetesBasic context m
  )
  -- | Path to @kubectl@ binary
  => FilePath
  -> KubernetesClusterContext
  -- | Whether to preload images
  -> Bool
  -- | Operator YAML
  -> Text
  -> (MinioOperatorContext -> m a)
  -> m a
withMinioOperator'' kubectlBinary kcc preloadImages allYaml action = do
  env <- getKubectlEnvironment kcc

  when preloadImages $ do
    let images = findAllImages (toText allYaml)

    forM_ images $ \image ->
      loadImageIfNecessary' kcc (ImageLoadSpecDocker image IfNotPresent)

  let create = createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString allYaml)
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
                                             env = Just env, delegate_ctlc = True }) (toString allYaml)
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
