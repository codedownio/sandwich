{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{-|

Record per-pod CPU and memory usage over the course of a test, using @kubectl top@.

A single @kubectl top pods@ sample reports both CPU and memory, so one watcher
captures both. Wrap an action in 'withResourceWatcher' and, when it finishes, these
artifacts are written into the current test folder:

  * @pod-resources.csv@ -- the raw time series (@elapsed_s,pod,cpu_m,mem_bytes@).
  * @pod-cpu-peak.txt@ -- each pod's peak CPU (millicores), sorted descending (also logged).
  * @pod-memory-peak.txt@ -- each pod's peak memory, sorted descending (also logged).
  * @pod-cpu.svg@ / @pod-memory.svg@ -- line charts over time, one line per pod.

CPU is reported in millicores, exactly as @kubectl top@ reports it (@1000m@ = one core).

This requires the [metrics-server](https://github.com/kubernetes-sigs/metrics-server)
to be installed (see "Test.Sandwich.Contexts.Kubernetes.MetricsServer"). If it
isn't, @kubectl top@ just fails, no samples are collected, and the watcher quietly
writes nothing -- so it's safe to wrap unconditionally.

-}

module Test.Sandwich.Contexts.Kubernetes.ResourceWatcher (
  -- * Spec-level (around)
  withResourceWatcher

  -- * Action-level (reader-based)
  , withResourceWatcher'

  -- * Action-level (explicit arguments)
  , withResourceWatcher''

  -- * Options
  , ResourceWatcherOptions(..)
  , defaultResourceWatcherOptions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import Relude
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.KubectlTop (parseContainerTopOutput, parseTopOutput)
import Test.Sandwich.Contexts.Kubernetes.Util.SeriesPlot (renderSeriesSvg)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception
import UnliftIO.Process


data ResourceWatcherOptions = ResourceWatcherOptions {
  -- | How often to sample @kubectl top@, in microseconds.
  resourceWatcherIntervalMicros :: Int
  -- | Whether to render the SVG charts (in addition to the CSV and peak summaries).
  , resourceWatcherWriteSvg :: Bool
  -- | When 'True', use @kubectl top pods --containers@ to break down usage
  -- by container within each pod. The identifier in output files becomes
  -- @pod\/container@ instead of just @pod@.
  , resourceWatcherContainerLevel :: Bool
  } deriving (Show, Eq)

-- | Sample every 5s and render the SVGs.
defaultResourceWatcherOptions :: ResourceWatcherOptions
defaultResourceWatcherOptions = ResourceWatcherOptions {
  resourceWatcherIntervalMicros = 5_000_000
  , resourceWatcherWriteSvg = True
  , resourceWatcherContainerLevel = False
  }

-- | (elapsedSeconds, cpuMillicores, memoryBytes)
type Sample = (Double, Integer, Integer)

-- | Around-style spec node that records per-pod CPU and memory for the duration
-- of the wrapped subtree, writing artifacts when it finishes. The spec-level
-- counterpart to the action-level 'withResourceWatcher'', mirroring
-- 'Test.Sandwich.Contexts.Kubernetes.Namespace.withKubernetesNamespace': drop it
-- in at the spec level to instrument a whole @describe@ / @it@ subtree.
withResourceWatcher :: (
  KubectlBasicWithoutReader context m
  )
  -- | Namespace to watch
  => Text
  -> ResourceWatcherOptions
  -> SpecFree context m ()
  -> SpecFree context m ()
withResourceWatcher namespace options = around [i|Watch pod CPU + memory in namespace '#{namespace}'|]
  (void . withResourceWatcher' namespace options)

-- | Run @action@ while sampling per-pod CPU and memory in @namespace@, writing
-- artifacts on completion. Reader-based version that obtains the cluster and
-- @kubectl@ binary from the context.
withResourceWatcher' :: (
  KubectlBasic context m
  )
  -- | Namespace to watch
  => Text
  -> ResourceWatcherOptions
  -> m a
  -> m a
withResourceWatcher' namespace options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withResourceWatcher'' kcc kubectlBinary namespace options action

-- | Same as 'withResourceWatcher'', but allows you to pass in the
-- 'KubernetesClusterContext' and @kubectl@ binary path.
withResourceWatcher'' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to the @kubectl@ binary
  -> FilePath
  -- | Namespace to watch
  -> Text
  -> ResourceWatcherOptions
  -> m a
  -> m a
withResourceWatcher'' kcc kubectlBinary namespace (ResourceWatcherOptions {..}) action = do
  env <- getKubectlEnvironment kcc
  startTime <- liftIO getCurrentTime
  samplesRef <- newIORef (M.empty :: Map Text [Sample])

  let topArgs = ["top", "pods", "-n", toString namespace, "--no-headers"]
               <> ["--containers" | resourceWatcherContainerLevel]
      parseRow = if resourceWatcherContainerLevel then parseContainerTopOutput else parseTopOutput

  let pollOnce = do
        now <- liftIO getCurrentTime
        let elapsed = realToFrac (diffUTCTime now startTime) :: Double
        (ec, out, _) <- readCreateProcessWithExitCode ((proc kubectlBinary topArgs) { env = Just env }) ""
        when (ec == ExitSuccess) $
          forM_ (parseRow (toText out)) $ \(key, cpuM, memBytes) ->
            modifyIORef' samplesRef (M.insertWith (\new old -> old <> new) key [(elapsed, cpuM, memBytes)])

  let loop = forever $ do
        handleAny (\e -> debug [i|(#{namespace}) kubectl top sample failed: #{e}|]) pollOnce
        threadDelay resourceWatcherIntervalMicros

  let writeArtifacts = do
        cleaned <- M.filter (not . null) <$> readIORef samplesRef
        if M.null cleaned
          then debug [i|(#{namespace}) No pod resource samples collected (is metrics-server installed?); skipping artifacts|]
          else getCurrentFolder >>= \case
            Nothing -> debug [i|(#{namespace}) No current folder; skipping resource artifacts|]
            Just dir -> writeAll dir cleaned

  finally (withAsync loop (const action)) writeArtifacts

  where
    writeAll dir cleaned = do
      -- Shared CSV with both columns.
      let csvRows = [ [i|#{fmtDouble t},#{pod},#{cpu},#{mem}|]
                    | (pod, pts) <- M.toList cleaned, (t, cpu, mem) <- pts ]
      liftIO $ writeFileText (dir </> "pod-resources.csv") (T.unlines ("elapsed_s,pod,cpu_m,mem_bytes" : csvRows))

      -- Peak CPU per pod (millicores), sorted descending.
      let cpuPeaks = sortOn (Down . snd) [ (pod, foldl' max 0 [c | (_, c, _) <- pts])
                                         | (pod, pts) <- M.toList cleaned ]
          cpuPeakText = T.unlines [ [i|#{cpu}m\t#{pod}|] | (pod, cpu) <- cpuPeaks ]
      liftIO $ writeFileText (dir </> "pod-cpu-peak.txt") cpuPeakText
      info [i|Peak pod CPU, millicores (namespace #{namespace}):\n#{cpuPeakText}|]

      -- Peak memory per pod, sorted descending.
      let memPeaks = sortOn (Down . snd) [ (pod, foldl' max 0 [m | (_, _, m) <- pts])
                                         | (pod, pts) <- M.toList cleaned ]
          memPeakText = T.unlines [ [i|#{fmtDouble (mibOf m)} MiB\t#{pod}|] | (pod, m) <- memPeaks ]
      liftIO $ writeFileText (dir </> "pod-memory-peak.txt") memPeakText
      info [i|Peak pod memory (namespace #{namespace}):\n#{memPeakText}|]

      -- SVG charts.
      when resourceWatcherWriteSvg $ do
        liftIO $ writeFileText (dir </> "pod-cpu.svg") $
          renderSeriesSvg "Pod CPU usage over time (millicores)" "No pod CPU samples were collected."
            (M.map (\pts -> [(t, fromIntegral c) | (t, c, _) <- pts]) cleaned)
        liftIO $ writeFileText (dir </> "pod-memory.svg") $
          renderSeriesSvg "Pod memory usage over time (MiB)" "No pod memory samples were collected."
            (M.map (\pts -> [(t, fromIntegral m / mib) | (t, _, m) <- pts]) cleaned)

    mib = 1024 * 1024 :: Double
    mibOf b = fromIntegral b / mib :: Double

-- | Format a 'Double' with a single decimal place.
fmtDouble :: Double -> Text
fmtDouble x = toText (showFFloat (Just 1) x "")
