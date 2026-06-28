{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

Record per-pod (or per-container) CPU, memory, and network usage over the course of a test by polling
the __kubelet summary API__ (@\/api\/v1\/nodes\/<node>\/proxy\/stats\/summary@, cAdvisor-backed) directly.

This needs __no metrics-server__ and pulls no images (the summary API is built into every kubelet). It
captures both the CPU __rate__ (@cpu.usageNanoCores@) and the __cumulative__ CPU counter
(@cpu.usageCoreNanoSeconds@) — so total CPU-seconds are accurate regardless of the sample interval
(rate sampling alone misses spikes between samples) — plus memory working set and per-pod network rx\/tx.

Caveat: cAdvisor cannot see inside __kata__ (VM-sandboxed) pods — they report zero. Run such pods on
the default runtime if you need to measure them.

Three entry points, sharing one core:

  * 'introduceResourceWatcher' — spec-level node that brackets a subtree and provides a
    'ResourceWatcher' handle to its children (so tests can drop timeline markers via 'markEvent').
  * 'withResourceWatcher' — action-level bracket you call inside an @it@ to monitor just that section.
  * 'withResourceWatcher'' — same, with the cluster context and @kubectl@ path passed in explicitly.

On completion these artifacts are written into the current test folder:

  * @pod-resources.csv@ — the raw time series
    (@elapsed_s,pod,cpu_core_ns,cpu_nanocores,mem_ws_bytes,net_rx_bytes,net_tx_bytes@).
  * @pod-resources-summary.json@ — derived per-pod totals (CPU-seconds, peak\/mean memory, net rx\/tx)
    plus overall aggregates.
  * @pod-cpu-peak.txt@ \/ @pod-memory-peak.txt@ — each pod's peak CPU\/memory, sorted descending (logged too).
  * @pod-resources-events.csv@ — timeline markers emitted via 'markEvent' (@elapsed_s,label@).
  * @pod-cpu.svg@ \/ @pod-memory.svg@ \/ @pod-network.svg@ — charts over time, one line per pod.
  * @pod-resources-report.html@ — a self-contained report stitching the charts, ranked tables, and events.

By default it watches all namespaces; set 'resourceWatcherNamespaces' to scope it. With
'resourceWatcherContainerLevel', each pod is broken down by container (identifier gains a
@\/container@ suffix; network stays pod-level and is reported as 0 for container rows).

-}

module Test.Sandwich.Contexts.Kubernetes.ResourceWatcher (
  -- * Spec-level (introduce)
  introduceResourceWatcher
  , resourceWatcher

  -- * Action-level (reader-based)
  , withResourceWatcher

  -- * Action-level (explicit arguments)
  , withResourceWatcher'

  -- * The handle
  , ResourceWatcher(..)
  , markEvent

  -- * Options
  , ResourceWatcherOptions(..)
  , defaultResourceWatcherOptions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Aeson (eitherDecodeStrict, encode, object, (.=))
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Lazy as BL
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
import Test.Sandwich.Contexts.Kubernetes.Util.SeriesPlot (renderSeriesSvg)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception
import UnliftIO.Process


-- | A handle, available to children of 'introduceResourceWatcher' (or returned by
-- 'withResourceWatcher'), for dropping labeled markers onto the resource timeline so a particular
-- test action can be correlated with the data.
newtype ResourceWatcher = ResourceWatcher {
  -- | Record a timeline marker at the current elapsed time. Prefer 'markEvent' from a test.
  resourceWatcherMark :: Text -> IO ()
  }

-- | Context label carrying the 'ResourceWatcher' handle under 'introduceResourceWatcher'.
resourceWatcher :: Label "resourceWatcher" ResourceWatcher
resourceWatcher = Label

-- | Drop a labeled marker onto the resource timeline (lifted 'resourceWatcherMark').
markEvent :: MonadIO m => ResourceWatcher -> Text -> m ()
markEvent rw = liftIO . resourceWatcherMark rw

data ResourceWatcherOptions = ResourceWatcherOptions {
  -- | Namespaces to watch. Empty (the default) means all namespaces.
  resourceWatcherNamespaces :: [Text]
  -- | How often to sample the summary API, in microseconds.
  , resourceWatcherIntervalMicros :: Int
  -- | When 'True', break each pod down by container (identifier gains a @\/container@ suffix).
  -- Network is pod-level only, so it is reported as 0 for container rows.
  , resourceWatcherContainerLevel :: Bool
  -- | Whether to render the SVG charts and the HTML report (in addition to the CSV/JSON/peaks).
  , resourceWatcherWriteReport :: Bool
  } deriving (Show, Eq)

-- | Watch all namespaces, sample every 5s, pod-level, and write the charts + report.
defaultResourceWatcherOptions :: ResourceWatcherOptions
defaultResourceWatcherOptions = ResourceWatcherOptions {
  resourceWatcherNamespaces = []
  , resourceWatcherIntervalMicros = 5_000_000
  , resourceWatcherContainerLevel = False
  , resourceWatcherWriteReport = True
  }

-- | One poll of a pod (or container): elapsed time plus the raw summary-API counters.
data Sample = Sample {
  sElapsed   :: !Double
  , sCpuCoreNs :: !Integer  -- ^ cumulative cpu.usageCoreNanoSeconds
  , sCpuNano   :: !Integer  -- ^ instantaneous cpu.usageNanoCores (rate)
  , sMem       :: !Integer  -- ^ memory.workingSetBytes
  , sRx        :: !Integer  -- ^ cumulative network.rxBytes (0 for container rows)
  , sTx        :: !Integer  -- ^ cumulative network.txBytes (0 for container rows)
  }

-- | Spec-level node: brackets a subtree, monitoring the configured namespaces for its duration, and
-- provides a 'ResourceWatcher' handle to children (via the 'resourceWatcher' context) so tests can
-- call 'markEvent'. Artifacts are written into the node's folder when it finishes.
introduceResourceWatcher :: (
  KubectlBasicWithoutReader context m
  )
  => ResourceWatcherOptions
  -> SpecFree (LabelValue "resourceWatcher" ResourceWatcher :> context) m ()
  -> SpecFree context m ()
introduceResourceWatcher options =
  introduceWith [i|Watch pod resources in #{scope options}|] resourceWatcher (void . withResourceWatcher options)

-- | Run @action@ (given a 'ResourceWatcher' handle) while sampling per-pod CPU/memory/network,
-- writing artifacts on completion. Obtains the cluster and @kubectl@ binary from the context.
withResourceWatcher :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context, HasFile context "kubectl"
  )
  => ResourceWatcherOptions
  -> (ResourceWatcher -> m a)
  -> m a
withResourceWatcher options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withResourceWatcher' kcc kubectlBinary options action

-- | Same as 'withResourceWatcher', but with the 'KubernetesClusterContext' and @kubectl@ path
-- passed in explicitly.
withResourceWatcher' :: (
  MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m
  )
  => KubernetesClusterContext
  -> FilePath
  -> ResourceWatcherOptions
  -> (ResourceWatcher -> m a)
  -> m a
withResourceWatcher' kcc kubectlBinary opts@(ResourceWatcherOptions {..}) action = do
  env <- getKubectlEnvironment kcc
  startTime <- liftIO getCurrentTime
  samplesRef <- newIORef (M.empty :: Map Text [Sample])
  eventsRef <- newIORef ([] :: [(Double, Text)])

  let rw = ResourceWatcher $ \label -> do
        now <- getCurrentTime
        modifyIORef' eventsRef ((realToFrac (diffUTCTime now startTime), label) :)

  let keep ns = null resourceWatcherNamespaces || ns `elem` resourceWatcherNamespaces

  -- Bound every kubectl call with --request-timeout so a poll can't block indefinitely. Without it, a
  -- call made while the cluster is tearing down can wedge, and cancelling the loop (withAsync) then
  -- blocks forever in cleanupProcess/waitForProcess, hanging the whole teardown. With it, kubectl exits
  -- on its own and the loop is always promptly cancellable.
  let reqTimeout = "--request-timeout=10s"
  let pollOnce = do
        now <- liftIO getCurrentTime
        let elapsed = realToFrac (diffUTCTime now startTime) :: Double
        (ec, nodesOut, _) <- readCreateProcessWithExitCode
          ((proc kubectlBinary ["get", "nodes", "-o", "name", reqTimeout]) { env = Just env }) ""
        when (ec == ExitSuccess) $
          forM_ (nodeNames (toText nodesOut)) $ \node -> do
            (ec2, out, _) <- readCreateProcessWithExitCode
              ((proc kubectlBinary ["get", "--raw", [i|/api/v1/nodes/#{node}/proxy/stats/summary|], reqTimeout]) { env = Just env }) ""
            when (ec2 == ExitSuccess) $
              forM_ (parseSamples resourceWatcherContainerLevel (encodeUtf8 (toText out))) $ \(ns, key, ccns, cn, mem, rx, tx) ->
                when (keep ns) $
                  modifyIORef' samplesRef (M.insertWith (<>) (ns <> "/" <> key) [Sample elapsed ccns cn mem rx tx])

  let loop = forever $ do
        handleAny (\e -> debug [i|(#{scope opts}) summary-api sample failed: #{e}|]) pollOnce
        threadDelay resourceWatcherIntervalMicros

  let writeArtifacts = do
        cleaned <- M.map (sortOn sElapsed) . M.filter (not . null) <$> readIORef samplesRef
        events <- sortOn fst <$> readIORef eventsRef
        if M.null cleaned
          then debug [i|(#{scope opts}) No pod resource samples collected; skipping artifacts|]
          else getCurrentFolder >>= \case
            Nothing -> debug [i|(#{scope opts}) No current folder; skipping resource artifacts|]
            Just dir -> writeAll opts dir cleaned events

  finally (withAsync loop (const (action rw))) writeArtifacts

-- | Label for the @namespace@ scope, used in log messages.
scope :: ResourceWatcherOptions -> Text
scope (ResourceWatcherOptions { resourceWatcherNamespaces = [] }) = "all namespaces"
scope (ResourceWatcherOptions { resourceWatcherNamespaces = nss }) = T.intercalate "," nss

-- | @node/<name>@ lines from @kubectl get nodes -o name@ → bare node names.
nodeNames :: Text -> [Text]
nodeNames = mapMaybe (T.stripPrefix "node/" . T.strip) . T.lines

-- | Pull @(namespace, key, cpuCoreNs, cpuNanoCores, memWs, rxBytes, txBytes)@ out of a summary-API
-- response. @key@ is @pod@ (or @pod/container@ when @containerLevel@). Containers carry no network,
-- so rx\/tx are 0 for them. Missing fields default to 0; malformed JSON yields no rows.
parseSamples :: Bool -> ByteString -> [(Text, Text, Integer, Integer, Integer, Integer, Integer)]
parseSamples containerLevel bs = case eitherDecodeStrict bs of
  Left _ -> []
  Right v -> concatMap perPod (asArray (lookupKey "pods" v))
  where
    perPod p =
      let ns = strAt ["podRef", "namespace"] p
          pod = strAt ["podRef", "name"] p
      in if T.null pod then [] else if containerLevel
        then [ (ns, pod <> "/" <> cn, intAt ["cpu", "usageCoreNanoSeconds"] c, intAt ["cpu", "usageNanoCores"] c
               , intAt ["memory", "workingSetBytes"] c, 0, 0)
             | c <- asArray (lookupKey "containers" p), let cn = strAt ["name"] c, not (T.null cn) ]
        else [ (ns, pod, intAt ["cpu", "usageCoreNanoSeconds"] p, intAt ["cpu", "usageNanoCores"] p
               , intAt ["memory", "workingSetBytes"] p, intAt ["network", "rxBytes"] p, intAt ["network", "txBytes"] p) ]
    asArray (Just (Array a)) = toList a
    asArray _ = []

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup (AK.fromText k) o
lookupKey _ _ = Nothing

intAt :: [Text] -> Value -> Integer
intAt [] (Number n) = round n
intAt (k:ks) (Object o) = maybe 0 (intAt ks) (KM.lookup (AK.fromText k) o)
intAt _ _ = 0

strAt :: [Text] -> Value -> Text
strAt [] (String s) = s
strAt (k:ks) (Object o) = maybe "" (strAt ks) (KM.lookup (AK.fromText k) o)
strAt _ _ = ""

-- | Write all artifacts for the collected (non-empty, time-sorted) per-pod series.
writeAll :: (MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m)
  => ResourceWatcherOptions -> FilePath -> Map Text [Sample] -> [(Double, Text)] -> m ()
writeAll opts dir cleaned events = do
  -- Raw time series.
  let csvRows = [ [i|#{fmt1 (sElapsed s)},#{pod},#{sCpuCoreNs s},#{sCpuNano s},#{sMem s},#{sRx s},#{sTx s}|]
                | (pod, pts) <- M.toList cleaned, s <- pts ]
  liftIO $ writeFileText (dir </> "pod-resources.csv")
    (T.unlines ("elapsed_s,pod,cpu_core_ns,cpu_nanocores,mem_ws_bytes,net_rx_bytes,net_tx_bytes" : csvRows))

  -- Per-pod derived totals (CPU-seconds from the cumulative counter; net totals = last-first).
  let derived = [ (pod, cpuSeconds pts, memPeak pts, memMean pts, deltaOf sRx pts, deltaOf sTx pts)
                | (pod, pts) <- M.toList cleaned ]
      byCpu = sortOn (\(_, c, _, _, _, _) -> Down c) derived
      totCpu = sum [c | (_, c, _, _, _, _) <- derived]
      totRx  = sum [r | (_, _, _, _, r, _) <- derived]
      totTx  = sum [t | (_, _, _, _, _, t) <- derived]

  liftIO $ BL.writeFile (dir </> "pod-resources-summary.json") $ encode $ object
    [ "pods" .= [ object [ "pod" .= pod, "cpu_seconds" .= c, "mem_peak_bytes" .= mp
                         , "mem_mean_bytes" .= mm, "net_rx_bytes" .= rx, "net_tx_bytes" .= tx ]
                | (pod, c, mp, mm, rx, tx) <- byCpu ]
    , "totals" .= object [ "cpu_seconds" .= totCpu, "net_rx_bytes" .= totRx, "net_tx_bytes" .= totTx ]
    , "events" .= [ object ["elapsed_s" .= e, "label" .= l] | (e, l) <- events ]
    ]

  liftIO $ writeFileText (dir </> "pod-resources-events.csv")
    (T.unlines ("elapsed_s,label" : [ [i|#{fmt1 e},#{l}|] | (e, l) <- events ]))

  -- Peak CPU (millicores) and memory (MiB) per pod, sorted descending (also logged).
  let cpuPeaks = sortOn (Down . snd) [ (pod, foldl' max 0 (map sCpuNano pts)) | (pod, pts) <- M.toList cleaned ]
      cpuPeakText = T.unlines [ [i|#{fmt1 (fromIntegral c / 1e6)}m\t#{pod}|] | (pod, c) <- cpuPeaks ]
  liftIO $ writeFileText (dir </> "pod-cpu-peak.txt") cpuPeakText
  info [i|Peak pod CPU, millicores (#{scope opts}):\n#{cpuPeakText}|]

  let memPeaks = sortOn (Down . snd) [ (pod, memPeak pts) | (pod, pts) <- M.toList cleaned ]
      memPeakText = T.unlines [ [i|#{fmtMiB m} MiB\t#{pod}|] | (pod, m) <- memPeaks ]
  liftIO $ writeFileText (dir </> "pod-memory-peak.txt") memPeakText
  info [i|Peak pod memory (#{scope opts}):\n#{memPeakText}|]

  when (resourceWatcherWriteReport opts) $ do
    let cpuSvg = renderSeriesSvg "Pod CPU usage over time (millicores)" "No CPU samples."
                   (M.map (\pts -> [(sElapsed s, fromIntegral (sCpuNano s) / 1e6) | s <- pts]) cleaned)
        memSvg = renderSeriesSvg "Pod memory working set over time (MiB)" "No memory samples."
                   (M.map (\pts -> [(sElapsed s, fromIntegral (sMem s) / mib) | s <- pts]) cleaned)
        netSvg = renderSeriesSvg "Pod cumulative network rx over time (MiB)" "No network samples."
                   (M.map (\pts -> [(sElapsed s, fromIntegral (sRx s) / mib) | s <- pts]) cleaned)
    liftIO $ writeFileText (dir </> "pod-cpu.svg") cpuSvg
    liftIO $ writeFileText (dir </> "pod-memory.svg") memSvg
    liftIO $ writeFileText (dir </> "pod-network.svg") netSvg
    liftIO $ writeFileText (dir </> "pod-resources-report.html") $
      renderReport byCpu (totCpu, totRx, totTx) events cpuSvg memSvg netSvg

  where
    mib = 1024 * 1024 :: Double
    cpuSeconds pts = fromIntegral (deltaOf sCpuCoreNs pts) / 1e9 :: Double
    -- cumulative counters are monotonic, so total over the window = max - min
    deltaOf f pts = let ys = map f pts; mx = foldl' max 0 ys in mx - foldl' min mx ys
    memPeak pts = foldl' max 0 (map sMem pts)
    memMean pts = if null pts then 0 else sum (map sMem pts) `div` fromIntegral (length pts)

-- | A self-contained HTML report: headline totals, ranked table, inline charts, event markers.
renderReport
  :: [(Text, Double, Integer, Integer, Integer, Integer)]  -- (pod, cpuSec, memPeak, memMean, rx, tx)
  -> (Double, Integer, Integer)                             -- (totalCpuSec, totalRx, totalTx)
  -> [(Double, Text)]                                       -- events
  -> Text -> Text -> Text                                   -- cpu/mem/net SVGs
  -> Text
renderReport rows (totCpu, totRx, totTx) events cpuSvg memSvg netSvg = [i|<!DOCTYPE html>
<html><head><meta charset="utf-8"><title>Pod resource report</title>
<style>body{font-family:sans-serif;margin:2em;max-width:1100px}table{border-collapse:collapse}
td,th{border:1px solid \#ccc;padding:3px 8px;text-align:right}th:first-child,td:first-child{text-align:left}
svg{max-width:100%;height:auto;border:1px solid \#eee;margin:1em 0}</style></head><body>
<h1>Pod resource report</h1>
<p><b>Total CPU:</b> #{fmt1 totCpu} core-seconds &nbsp; <b>Total net rx:</b> #{fmtMiB totRx} MiB &nbsp; <b>Total net tx:</b> #{fmtMiB totTx} MiB</p>
<h2>Per-pod totals (by CPU-seconds)</h2>
<table><tr><th>pod</th><th>CPU (core-s)</th><th>mem peak (MiB)</th><th>mem mean (MiB)</th><th>net rx (MiB)</th><th>net tx (MiB)</th></tr>
#{podRows}
</table>
<h2>Events</h2>
<table><tr><th>elapsed (s)</th><th>label</th></tr>
#{eventRows}
</table>
<h2>CPU</h2>#{cpuSvg}
<h2>Memory</h2>#{memSvg}
<h2>Network (cumulative rx)</h2>#{netSvg}
</body></html>
|]
  where
    podRows = T.concat [ tr [pod, fmt1 c, fmtMiB mp, fmtMiB mm, fmtMiB rx, fmtMiB tx]
                       | (pod, c, mp, mm, rx, tx) <- rows ]
    eventRows = T.concat [ tr [fmt1 e, l] | (e, l) <- events ]
    tr cells = "<tr>" <> T.concat ["<td>" <> c <> "</td>" | c <- cells] <> "</tr>"

fmt1 :: Double -> Text
fmt1 x = toText (showFFloat (Just 1) x "")

fmtMiB :: Integer -> Text
fmtMiB b = fmt1 (fromIntegral b / (1024 * 1024))
