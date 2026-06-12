{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Demo exercising the three Kubernetes instrumentation helpers together:
--
--   * 'introduceMetricsServer' (installs metrics-server so @kubectl top@ works),
--   * 'withResourceWatcher' (records per-pod CPU + memory over time -> CSV + peaks + SVGs), and
--   * 'withOOMWatcher' / 'findOOMKilled'' (detects OOMKilled containers).
--
-- Run it with e.g.:
--
-- > stack run demo-kubernetes-instrumentation -- --print
--
-- It brings up a Minikube cluster, confirms @kubectl top@ works, samples a small
-- memory-using workload for a minute (writing artifacts into the test tree), and
-- then deliberately OOM-kills a pod to show the OOM detection firing.

module Main where

import Control.Monad.Logger (MonadLoggerIO)
import Data.String.Interpolate
import Relude
import System.Exit (ExitCode(..))
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Kubernetes.OOMWatcher (findOOMKilled')
import Test.Sandwich.Contexts.Nix
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process


demoNamespace :: Text
demoNamespace = "instrumentation-demo"

oomNamespace :: Text
oomNamespace = "instrumentation-oom"

spec :: TopSpec
spec = describe "Kubernetes instrumentation demo" $
  introduceNixContext nixpkgsReleaseDefault $
  introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $
  -- Minikube doesn't install metrics-server itself, so install it explicitly.
  -- (introduceMetricsServer also brings in a kubectl binary via Nix for children.)
  introduceMetricsServer defaultMetricsServerOptions $ do

    it "metrics-server is serving `kubectl top`" $ do
      (kubectl, env) <- askKubectlArgs
      out <- readCreateProcess ((proc kubectl ["top", "pods", "--all-namespaces"]) { env = Just env }) ""
      info [i|metrics-server is up. `kubectl top pods --all-namespaces`:\n#{toText out}|]

    -- (c) resource watcher + (a) OOM watcher, both scoped to the workload namespace.
    withKubernetesNamespace demoNamespace $
      it "records per-pod CPU + memory while watching for OOMKills" $ do
        info [i|Deploying a memory-using workload into namespace '#{demoNamespace}'|]
        kubectlApply (workloadYaml demoNamespace)
        kubectlAssert "rollout" ["rollout", "status", "deployment/memory-user"
                                , "-n", toString demoNamespace, "--timeout=180s"]

        info [i|Sampling pod CPU + memory for 60s (watching for OOMKills the whole time)...|]
        withOOMWatcher demoNamespace $
          withResourceWatcher' demoNamespace defaultResourceWatcherOptions $
            threadDelay 60_000_000

        getCurrentFolder >>= \case
          Just dir -> info [i|Wrote pod-resources.csv, pod-{cpu,memory}-peak.txt and pod-{cpu,memory}.svg to: #{dir}|]
          Nothing -> info [i|(no current folder, so no artifacts were written)|]

    -- (a) OOM detection, demonstrated positively (without failing the run).
    withKubernetesNamespace oomNamespace $
      it "detects an OOMKilled pod" $ do
        info [i|Deploying a pod that will exceed its memory limit in namespace '#{oomNamespace}'|]
        kubectlApply (oomYaml oomNamespace)

        kcc <- getContext kubernetesCluster
        (kubectl, _) <- askKubectlArgs
        findOOMWithTimeout kcc kubectl oomNamespace 40 >>= \case
          Just detail -> info [i|Detected the OOMKilled pod as expected:\n#{detail}|]
          Nothing -> expectationFailure [i|Expected an OOMKilled pod in '#{oomNamespace}' but none was detected|]


-- | Apply a manifest by piping it to @kubectl apply -f -@.
kubectlApply :: (KubectlBasic context m, MonadFail m) => Text -> m ()
kubectlApply yaml = do
  (kubectl, env) <- askKubectlArgs
  p <- createProcessWithFileLoggingAndStdin' "kubectl-apply" ((proc kubectl ["apply", "-f", "-"]) { env = Just env }) (toString yaml)
  waitForProcess p >>= (`shouldBe` ExitSuccess)

-- | Run a @kubectl@ command and assert it succeeds.
kubectlAssert :: (KubectlBasic context m) => String -> [String] -> m ()
kubectlAssert name args = do
  (kubectl, env) <- askKubectlArgs
  p <- createProcessWithFileLogging' name ((proc kubectl args) { env = Just env })
  waitForProcess p >>= (`shouldBe` ExitSuccess)

-- | Poll 'findOOMKilled'' until it reports an OOMKill or we run out of attempts
-- (3s apart).
findOOMWithTimeout :: (
  MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m
  ) => KubernetesClusterContext -> FilePath -> Text -> Int -> m (Maybe String)
findOOMWithTimeout kcc kubectl ns attempts
  | attempts <= 0 = return Nothing
  | otherwise = findOOMKilled' kcc kubectl ns >>= \case
      Just d -> return (Just d)
      Nothing -> threadDelay 3_000_000 >> findOOMWithTimeout kcc kubectl ns (attempts - 1)


-- | A small Deployment that holds ~128Mi per replica, well under its limit.
workloadYaml :: Text -> Text
workloadYaml ns = [i|apiVersion: apps/v1
kind: Deployment
metadata:
  name: memory-user
  namespace: #{ns}
spec:
  replicas: 2
  selector:
    matchLabels: {app: memory-user}
  template:
    metadata:
      labels: {app: memory-user}
    spec:
      containers:
      - name: stress
        image: polinux/stress:latest
        command: ["stress"]
        args: ["--vm", "1", "--vm-bytes", "128M", "--vm-hang", "1000"]
        resources:
          requests: {memory: "64Mi"}
          limits: {memory: "256Mi"}
|]

-- | A pod that tries to allocate 250M with a 32Mi limit, so it gets OOMKilled.
oomYaml :: Text -> Text
oomYaml ns = [i|apiVersion: v1
kind: Pod
metadata:
  name: oom-victim
  namespace: #{ns}
spec:
  restartPolicy: Never
  containers:
  - name: stress
    image: polinux/stress:latest
    command: ["stress"]
    args: ["--vm", "1", "--vm-bytes", "250M", "--vm-hang", "1000"]
    resources:
      limits: {memory: "32Mi"}
|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
