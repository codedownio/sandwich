{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|

Install [Typesense](https://typesense.org/) on a Kubernetes cluster using Helm.

Typesense is an open-source, typo-tolerant search engine optimized for instant search experiences.

-}

module Test.Sandwich.Contexts.Kubernetes.Typesense (
  -- * Introduce Typesense
  introduceTypesense

  -- * Bracket-style versions
  , withTypesense
  , withTypesense'

  -- * Types
  , TypesenseOptions(..)
  , defaultTypesenseOptions

  , typesense
  , TypesenseContext(..)
  , HasTypesenseContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import Relude hiding (withFile)
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix
import UnliftIO.Exception
import UnliftIO.Process


-- * Types

data TypesenseContext = TypesenseContext {
  typesenseOptions :: TypesenseOptions
  -- | The service name that can be used to connect to Typesense within the cluster
  , typesenseServiceName :: Text
  -- | The namespace where Typesense is installed
  , typesenseNamespace :: Text
  } deriving (Show)

data TypesenseOptions = TypesenseOptions {
  -- | Helm chart to install (repo URL or OCI reference)
  typesenseHelmChart :: Text
  -- | Release name for Helm
  , typesenseReleaseName :: Text
  -- | API key for Typesense authentication
  , typesenseApiKey :: Text
  -- | Number of replicas (for HA setup)
  , typesenseReplicas :: Int
  -- | Storage size for persistence
  , typesenseStorageSize :: Text
  -- | Extra arguments to pass to Helm
  , typesenseHelmArgs :: [String]
  } deriving (Show)

defaultTypesenseOptions :: TypesenseOptions
defaultTypesenseOptions = TypesenseOptions {
  typesenseHelmChart = "springboard/typesense"
  , typesenseReleaseName = "typesense"
  , typesenseApiKey = "xyz"
  , typesenseReplicas = 1
  , typesenseStorageSize = "1Gi"
  , typesenseHelmArgs = []
  }

typesense :: Label "typesense" TypesenseContext
typesense = Label
type HasTypesenseContext context = HasLabel context "typesense" TypesenseContext

type ContextWithTypesense context =
  LabelValue "typesense" TypesenseContext
  :> context


-- * Introduce

-- | Introduce [Typesense](https://typesense.org/) on the Kubernetes cluster, in a given namespace.
introduceTypesense :: (
  KubernetesClusterBasicWithoutReader context m
  , HasNixContext context
  , HasFile context "kubectl"
  )
  -- | Namespace
  => Text
  -> TypesenseOptions
  -> SpecFree (ContextWithTypesense context) m ()
  -> SpecFree context m ()
introduceTypesense namespace options =
  introduceWith "introduce Typesense" typesense (void . withTypesense namespace options)

-- | Bracket-style version of 'introduceTypesense'.
withTypesense :: forall context m a. (
  HasCallStack, MonadFail m
  , KubectlBasic context m, HasNixContext context
  )
  -- | Namespace
  => Text
  -> TypesenseOptions
  -> (TypesenseContext -> m a)
  -> m a
withTypesense namespace options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withTypesense' kcc kubectlBinary namespace options action

-- | Same as 'withTypesense', but allows you to pass in the 'KubernetesClusterContext' and @kubectl@ binary path.
withTypesense' :: forall context m a. (
  HasCallStack, MonadFail m, NixContextBasic context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -- | Namespace
  -> Text
  -> TypesenseOptions
  -> (TypesenseContext -> m a)
  -> m a
withTypesense' kcc _kubectlBinary namespace options@(TypesenseOptions {..}) action = do
  helmBinary <- buildNixSymlinkJoin ["kubernetes-helm"] >>= \p -> return (p <> "/bin/helm")

  env <- getKubectlEnvironment kcc

  -- Add the Helm repository
  info [i|Adding Typesense Helm repository...|]
  createProcessWithLogging ((proc helmBinary [
    "repo", "add", "springboard", "https://helm-charts.springboardvr.com"
    ]) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  createProcessWithLogging ((proc helmBinary ["repo", "update"]) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- Install Typesense via Helm
  info [i|Installing Typesense via Helm in namespace '#{namespace}'...|]
  let helmArgs = [
        "install", toString typesenseReleaseName
        , toString typesenseHelmChart
        , "--namespace", toString namespace
        , "--create-namespace"
        , "--wait"
        , "--timeout", "5m"
        , "--atomic"
        , "--set", [i|replicas=#{typesenseReplicas}|]
        , "--set", [i|persistence.size=#{typesenseStorageSize}|]
        -- API key must be passed via extraEnv
        , "--set", [i|extraEnv[0].name=TYPESENSE_API_KEY|]
        , "--set", [i|extraEnv[0].value=#{typesenseApiKey}|]
        ] <> typesenseHelmArgs

  info [i|helm #{T.intercalate " " (fmap toText helmArgs)}|]

  createProcessWithLogging ((proc helmBinary helmArgs) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  let serviceName = typesenseReleaseName

  info [i|Typesense installed successfully. Service: #{serviceName}|]

  flip finally (cleanupTypesense helmBinary env namespace typesenseReleaseName) $ do
    action $ TypesenseContext {
      typesenseOptions = options
      , typesenseServiceName = serviceName
      , typesenseNamespace = namespace
      }

cleanupTypesense :: (MonadLoggerIO m, MonadUnliftIO m) => FilePath -> [(String, String)] -> Text -> Text -> m ()
cleanupTypesense helmBinary env namespace releaseName = do
  info [i|Cleaning up Typesense release '#{releaseName}' in namespace '#{namespace}'...|]
  createProcessWithLogging ((proc helmBinary [
    "uninstall", toString releaseName
    , "--namespace", toString namespace
    ]) { env = Just env })
    >>= waitForProcess >>= \case
      ExitSuccess -> info [i|Typesense uninstalled successfully.|]
      ExitFailure n -> warn [i|Failed to uninstall Typesense (exit code #{n})|]
