{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

Install the [Kubernetes metrics-server](https://github.com/kubernetes-sigs/metrics-server)
onto a cluster, so that the [Metrics API](https://kubernetes.io/docs/tasks/debug/debug-cluster/resource-metrics-pipeline/)
(@kubectl top@, @metrics.k8s.io@) becomes available.

This is handy together with "Test.Sandwich.Contexts.Kubernetes.ResourceWatcher",
which uses @kubectl top@ to record per-pod CPU and memory usage over a test.

For test clusters (kind/Minikube) the kubelet's serving certificate isn't signed
by the cluster CA, so metrics-server is configured with @--kubelet-insecure-tls@
by default ('metricsServerKubeletInsecureTls').

If you run tests that forbid unexpected image pulls (e.g. for hermeticity),
preload the metrics-server image onto the cluster yourself (e.g. with
'Test.Sandwich.Contexts.Kubernetes.Images.loadImageIfNecessary'') before calling
this, and point 'metricsServerManifestSource' at a locally-fetched manifest whose
image ref matches the preloaded image.

-}

module Test.Sandwich.Contexts.Kubernetes.MetricsServer (
  -- * Install
  installMetricsServer
  , installMetricsServer'

  -- * Spec-tree helpers
  , introduceMetricsServer
  , withMetricsServer

  -- * Options
  , MetricsServerOptions(..)
  , MetricsServerManifestSource(..)
  , defaultMetricsServerOptions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix (HasNixContext)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process


-- | Where to obtain the metrics-server manifest (the @components.yaml@).
data MetricsServerManifestSource =
  -- | Apply the manifest directly from a URL (default: the official release).
  MetricsServerManifestUrl Text
  -- | Apply a manifest from a local file (e.g. one fetched/built via Nix).
  | MetricsServerManifestFile FilePath
  deriving (Show, Eq)

data MetricsServerOptions = MetricsServerOptions {
  -- | Where to get the @components.yaml@ manifest.
  metricsServerManifestSource :: MetricsServerManifestSource
  -- | Whether to add @--kubelet-insecure-tls@ to the metrics-server args. Needed
  -- on kind/Minikube, whose kubelet serving certs aren't signed by the cluster CA.
  , metricsServerKubeletInsecureTls :: Bool
  -- | How long to wait (seconds) for the deployment rollout and for the metrics
  -- API to start serving.
  , metricsServerWaitTimeoutSeconds :: Double
  } deriving (Show, Eq)

-- | Default options: install metrics-server v0.7.2 from the official release URL,
-- with @--kubelet-insecure-tls@ and a 120s wait.
defaultMetricsServerOptions :: MetricsServerOptions
defaultMetricsServerOptions = MetricsServerOptions {
  metricsServerManifestSource = MetricsServerManifestUrl "https://github.com/kubernetes-sigs/metrics-server/releases/download/v0.7.2/components.yaml"
  , metricsServerKubeletInsecureTls = True
  , metricsServerWaitTimeoutSeconds = 120
  }

-- | Type alias for a context with a @kubectl@ binary added.
type ContextWithKubectl context = LabelValue "file-kubectl" (EnvironmentFile "kubectl") :> context

-- | Install metrics-server onto the current cluster, deriving the cluster and
-- @kubectl@ binary from the context.
installMetricsServer :: (
  KubectlBasic context m
  )
  => MetricsServerOptions
  -> m ()
installMetricsServer options = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  installMetricsServer' kcc kubectlBinary options

-- | Same as 'installMetricsServer', but allows you to pass in the
-- 'KubernetesClusterContext' and @kubectl@ binary path.
installMetricsServer' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to the @kubectl@ binary
  -> FilePath
  -> MetricsServerOptions
  -> m ()
installMetricsServer' kcc kubectlBinary (MetricsServerOptions {..}) = do
  env <- getKubectlEnvironment kcc

  let runK name args = do
        ps <- createProcessWithFileLogging' name ((proc kubectlBinary args) { env = Just env })
        waitForProcess ps >>= (`shouldBe` ExitSuccess)

  let manifestArg = case metricsServerManifestSource of
        MetricsServerManifestUrl url -> toString url
        MetricsServerManifestFile path -> path

  info [i|Installing metrics-server from #{manifestArg}|]
  runK "metrics-server-apply" ["apply", "-f", manifestArg]

  when metricsServerKubeletInsecureTls $ do
    info [i|Patching metrics-server with --kubelet-insecure-tls|]
    runK "metrics-server-patch" [
      "patch", "-n", "kube-system", "deployment", "metrics-server", "--type=json"
      , "-p", [i|[{"op":"add","path":"/spec/template/spec/containers/0/args/-","value":"--kubelet-insecure-tls"}]|]
      ]

  info [i|Waiting for metrics-server rollout|]
  runK "metrics-server-rollout" [
    "rollout", "status", "deployment/metrics-server", "-n", "kube-system"
    , [i|--timeout=#{round metricsServerWaitTimeoutSeconds :: Int}s|]
    ]

  -- The metrics pipeline takes a little while to start serving after the
  -- deployment is available; poll `kubectl top` until it succeeds.
  let intervalMicros = 3_000_000
  let attempts = max 1 (round (metricsServerWaitTimeoutSeconds * 1_000_000 / fromIntegral intervalMicros)) :: Int
  let topReady = do
        (ec, _, _) <- readCreateProcessWithExitCode ((proc kubectlBinary ["top", "pods", "-n", "kube-system"]) { env = Just env }) ""
        return (ec == ExitSuccess)
  let waitTop n
        | n <= 0 = warn [i|metrics-server did not start serving `kubectl top` within #{metricsServerWaitTimeoutSeconds}s; continuing anyway|]
        | otherwise = topReady >>= \case
            True -> info [i|metrics-server is serving pod metrics|]
            False -> threadDelay intervalMicros >> waitTop (n - 1)
  waitTop attempts

-- | Bracket-style: install metrics-server, then run the action. (Nothing to tear
-- down -- the cluster is ephemeral.)
withMetricsServer :: (
  KubectlBasic context m
  )
  => MetricsServerOptions
  -> m a
  -> m a
withMetricsServer options action = installMetricsServer options >> action

-- | Introduce metrics-server: bring in a @kubectl@ binary via Nix and install
-- metrics-server before running the child spec. Use this nested inside a cluster
-- introduction (e.g. 'Test.Sandwich.Contexts.Kubernetes.introduceKindClusterViaNix').
introduceMetricsServer :: (
  HasBaseContext context, HasNixContext context, HasKubernetesClusterContext context
  , MonadUnliftIO m
  )
  => MetricsServerOptions
  -> SpecFree (ContextWithKubectl context) m ()
  -> SpecFree context m ()
introduceMetricsServer options spec =
  introduceBinaryViaNixPackage @"kubectl" "kubectl" $
    before "Install metrics-server" (installMetricsServer options) spec
