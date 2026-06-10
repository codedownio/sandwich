{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{-|

Record per-pod memory usage over the course of a test, using @kubectl top@.

Wrap an action in 'withMemoryWatcher' and, when it finishes, three artifacts are
written into the current test folder:

  * @pod-memory.csv@ -- the raw time series (@elapsed_s,pod,cpu_m,mem_bytes@).
  * @pod-memory-peak.txt@ -- each pod's peak memory, sorted descending (also logged).
  * @pod-memory.svg@ -- a line chart of memory over time, one line per pod.

This requires the [metrics-server](https://github.com/kubernetes-sigs/metrics-server)
to be installed (see "Test.Sandwich.Contexts.Kubernetes.MetricsServer"). If it
isn't, @kubectl top@ just fails, no samples are collected, and the watcher quietly
writes nothing -- so it's safe to wrap unconditionally.

-}

module Test.Sandwich.Contexts.Kubernetes.MemoryWatcher (
  -- * Reader-based
  withMemoryWatcher

  -- * Explicit-argument variant
  , withMemoryWatcher'

  -- * Options
  , MemoryWatcherOptions(..)
  , defaultMemoryWatcherOptions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Ord (Down(..))
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
import Test.Sandwich.Contexts.Kubernetes.Util.MemoryPlot (renderMemorySvg)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception
import UnliftIO.Process


data MemoryWatcherOptions = MemoryWatcherOptions {
  -- | How often to sample @kubectl top@, in microseconds.
  memoryWatcherIntervalMicros :: Int
  -- | Whether to render the SVG chart (in addition to the CSV and peak summary).
  , memoryWatcherWriteSvg :: Bool
  } deriving (Show, Eq)

-- | Sample every 5s and render the SVG.
defaultMemoryWatcherOptions :: MemoryWatcherOptions
defaultMemoryWatcherOptions = MemoryWatcherOptions {
  memoryWatcherIntervalMicros = 5_000_000
  , memoryWatcherWriteSvg = True
  }

-- | (elapsedSeconds, cpuMillicores, memoryBytes)
type Sample = (Double, Integer, Integer)

-- | Run @action@ while sampling per-pod memory in @namespace@, writing artifacts
-- on completion. Reader-based version that obtains the cluster and @kubectl@
-- binary from the context.
withMemoryWatcher :: (
  KubectlBasic context m
  )
  -- | Namespace to watch
  => Text
  -> MemoryWatcherOptions
  -> m a
  -> m a
withMemoryWatcher namespace options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withMemoryWatcher' kcc kubectlBinary namespace options action

-- | Same as 'withMemoryWatcher', but allows you to pass in the
-- 'KubernetesClusterContext' and @kubectl@ binary path.
withMemoryWatcher' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to the @kubectl@ binary
  -> FilePath
  -- | Namespace to watch
  -> Text
  -> MemoryWatcherOptions
  -> m a
  -> m a
withMemoryWatcher' kcc kubectlBinary namespace (MemoryWatcherOptions {..}) action = do
  env <- getKubectlEnvironment kcc
  startTime <- liftIO getCurrentTime
  samplesRef <- newIORef (M.empty :: Map Text [Sample])

  let pollOnce = do
        now <- liftIO getCurrentTime
        let elapsed = realToFrac (diffUTCTime now startTime) :: Double
        (ec, out, _) <- readCreateProcessWithExitCode ((proc kubectlBinary ["top", "pods", "-n", toString namespace, "--no-headers"]) { env = Just env }) ""
        when (ec == ExitSuccess) $
          forM_ (parseTopOutput (toText out)) $ \(pod, cpuM, memBytes) ->
            modifyIORef' samplesRef (M.insertWith (\new old -> old <> new) pod [(elapsed, cpuM, memBytes)])

  let loop = forever $ do
        handleAny (\e -> debug [i|(#{namespace}) kubectl top sample failed: #{e}|]) pollOnce
        threadDelay memoryWatcherIntervalMicros

  let writeArtifacts = do
        cleaned <- M.filter (not . null) <$> readIORef samplesRef
        if M.null cleaned
          then debug [i|(#{namespace}) No pod memory samples collected (is metrics-server installed?); skipping artifacts|]
          else getCurrentFolder >>= \case
            Nothing -> debug [i|(#{namespace}) No current folder; skipping memory artifacts|]
            Just dir -> writeAll dir cleaned

  finally (withAsync loop (const action)) writeArtifacts

  where
    writeAll dir cleaned = do
      -- CSV
      let csvRows = [ [i|#{fmtDouble t},#{pod},#{cpu},#{mem}|]
                    | (pod, pts) <- M.toList cleaned, (t, cpu, mem) <- pts ]
      liftIO $ writeFileText (dir </> "pod-memory.csv") (T.unlines ("elapsed_s,pod,cpu_m,mem_bytes" : csvRows))

      -- Peak per pod, sorted descending
      let peaks = sortOn (Down . snd) [ (pod, foldl' max 0 [m | (_, _, m) <- pts])
                                      | (pod, pts) <- M.toList cleaned ]
      let peakText = T.unlines [ [i|#{fmtDouble (mibOf m)} MiB\t#{pod}|] | (pod, m) <- peaks ]
      liftIO $ writeFileText (dir </> "pod-memory-peak.txt") peakText
      info [i|Peak pod memory (namespace #{namespace}):\n#{peakText}|]

      -- SVG
      when memoryWatcherWriteSvg $
        liftIO $ writeFileText (dir </> "pod-memory.svg")
          (renderMemorySvg (M.map (\pts -> [(t, m) | (t, _, m) <- pts]) cleaned))

    mibOf b = fromIntegral b / (1024 * 1024) :: Double

-- | Format a 'Double' with a single decimal place.
fmtDouble :: Double -> Text
fmtDouble x = toText (showFFloat (Just 1) x "")

-- | Parse the output of @kubectl top pods --no-headers@ into
-- @(pod, cpuMillicores, memoryBytes)@ triples.
parseTopOutput :: Text -> [(Text, Integer, Integer)]
parseTopOutput = mapMaybe parseLine . lines
  where
    parseLine l = case words l of
      (name : cpu : mem : _) -> Just (name, parseCpu cpu, parseMem mem)
      _ -> Nothing

-- | Parse a kubectl CPU quantity (e.g. @12m@, @1@) into millicores.
parseCpu :: Text -> Integer
parseCpu t = case T.stripSuffix "m" t of
  Just n -> fromMaybe 0 (readMaybe (toString n))
  Nothing -> maybe 0 (\d -> round (d * 1000)) (readMaybe (toString t) :: Maybe Double)

-- | Parse a kubectl memory quantity (e.g. @345Mi@, @1Gi@, @512Ki@) into bytes.
parseMem :: Text -> Integer
parseMem t = fromMaybe 0 $ asum [
  parseWith "Ki" 1024
  , parseWith "Mi" (1024 * 1024)
  , parseWith "Gi" (1024 * 1024 * 1024)
  , parseWith "Ti" (1024 * 1024 * 1024 * 1024)
  , parseWith "k" 1000
  , parseWith "M" 1000000
  , parseWith "G" 1000000000
  , readMaybe (toString t)  -- plain bytes
  ]
  where
    parseWith suf mult = do
      n <- T.stripSuffix suf t
      d <- readMaybe (toString n) :: Maybe Double
      Just (round (d * mult))
