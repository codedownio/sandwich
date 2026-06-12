{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{-|

Detect [OOMKilled](https://kubernetes.io/docs/concepts/scheduling-eviction/node-pressure-eviction/)
containers in a Kubernetes cluster.

A container OOMKill is almost always a memory/resource misconfiguration (a pod's
memory limit too low, or the node overcommitted), and it tends to surface as a
baffling downstream flake -- e.g. a server pod gets OOM-killed, loses readiness,
its Service endpoint is removed, kube-proxy installs a "no endpoints" REJECT for
the NodePort, and clients start getting "connection refused". These helpers let
you detect it explicitly and fail the test instead.

By default the watch covers all namespaces; set 'oomWatcherNamespace' to scope it
to one (e.g. so a test that deliberately OOMKills a pod elsewhere doesn't trip it).

-}

module Test.Sandwich.Contexts.Kubernetes.OOMWatcher (
  -- * Spec-level (around)
  withOOMWatcher

  -- * Reader-based (action-level)
  , checkForOOMKills
  , withOOMWatcher'

  -- * Explicit-argument variants
  , findOOMKilled'
  , checkForOOMKills'
  , withOOMWatcher''

  -- * Options
  , OOMWatcherOptions(..)
  , defaultOOMWatcherOptions
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Relude
import System.IO (hGetLine)
import System.Process (cleanupProcess)
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.KubectlTop (namespaceArgs)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception
import UnliftIO.Process


data OOMWatcherOptions = OOMWatcherOptions {
  -- | Namespace to watch, or all namespaces when 'Nothing' (the default).
  oomWatcherNamespace :: Maybe Text
  } deriving (Show, Eq)

-- | Watch all namespaces.
defaultOOMWatcherOptions :: OOMWatcherOptions
defaultOOMWatcherOptions = OOMWatcherOptions {
  oomWatcherNamespace = Nothing
  }

-- | Human-readable description of the watched scope.
scopeLabel :: Maybe Text -> Text
scopeLabel = maybe "all namespaces" (\ns -> [i|namespace '#{ns}'|])

-- | One-shot: fail if any container is/was OOMKilled. Reader-based version that
-- obtains the cluster and @kubectl@ binary from the context.
checkForOOMKills :: (
  KubectlBasic context m
  )
  -- | Namespace to check, or all namespaces when 'Nothing'
  => Maybe Text
  -> m ()
checkForOOMKills namespace = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  checkForOOMKills' kcc kubectlBinary namespace

-- | Around-style spec node: run the wrapped subtree with a background pod-status
-- watch, failing the run at the end if any container is observed OOMKilled. The
-- spec-level counterpart to the action-level 'withOOMWatcher'', mirroring
-- 'Test.Sandwich.Contexts.Kubernetes.Namespace.withKubernetesNamespace'.
withOOMWatcher :: (
  KubectlBasicWithoutReader context m
  )
  => OOMWatcherOptions
  -> SpecFree context m ()
  -> SpecFree context m ()
withOOMWatcher options = around [i|Watch for OOMKills in #{scopeLabel (oomWatcherNamespace options)}|]
  (void . withOOMWatcher' options)

-- | Run @action@ with a background pod-status watch, failing the run at the end
-- if any container is observed OOMKilled. Reader-based version.
withOOMWatcher' :: (
  KubectlBasic context m
  )
  => OOMWatcherOptions
  -> m a
  -> m a
withOOMWatcher' options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withOOMWatcher'' kcc kubectlBinary options action

-- | Returns a human-readable pod listing if any container is (or was, via
-- @lastState@) OOMKilled, else Nothing. Checks one namespace, or all when 'Nothing'.
findOOMKilled' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> FilePath -> Maybe Text -> m (Maybe String)
findOOMKilled' kcc kubectlBinary namespace = do
  env <- getKubectlEnvironment kcc
  let runK args = readCreateProcessWithFileLogging ((proc kubectlBinary args) { env = Just env }) ""

  reasons <- runK (["get", "pods"] <> namespaceArgs namespace <> ["-o"
                  , "jsonpath={range .items[*]}{range .status.containerStatuses[*]}{.state.terminated.reason} {.lastState.terminated.reason} {end}{end}"])

  if "OOMKilled" `L.isInfixOf` reasons
    then Just <$> runK (["get", "pods"] <> namespaceArgs namespace <> ["-o", "wide"])
    else return Nothing

-- | One-shot: fail if any container is/was OOMKilled (one namespace, or all when 'Nothing').
checkForOOMKills' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> FilePath -> Maybe Text -> m ()
checkForOOMKills' kcc kubectlBinary namespace =
  findOOMKilled' kcc kubectlBinary namespace >>= \case
    Nothing -> return ()
    Just detail -> expectationFailure [i|Detected an OOMKilled container in #{scopeLabel namespace} (memory/resource misconfiguration). Pods:\n#{detail}|]

-- | Run @action@ with a background pod-status *watch*, failing the run at the end
-- if any container is observed OOMKilled.
--
-- Container OOMKills produce no kubectl event (verified empirically) -- the only
-- signal is @.status.containerStatuses[].{state,lastState}.terminated.reason@. So
-- instead of polling, we stream @kubectl get pods --watch@ (default table output,
-- which streams one row per watch event; the row's STATUS column shows
-- "OOMKilled"). The apiserver pushes a row whenever a pod's status changes, so we
-- see an OOMKilled the instant kubelet sets it, including for short-lived pods
-- (e.g. runner pods) that are deleted right after. We scan each streamed row for
-- "OOMKilled" and re-establish the watch if it drops. (NB: we deliberately do not
-- use @-o jsonpath@ -- its @--watch@ only prints the initial state, not updates.)
withOOMWatcher'' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> FilePath -> OOMWatcherOptions -> m a -> m a
withOOMWatcher'' kcc kubectlBinary (OOMWatcherOptions {..}) action = do
  env <- getKubectlEnvironment kcc
  oomRef <- newIORef Nothing

  let scope = scopeLabel oomWatcherNamespace
  let nsArgs = namespaceArgs oomWatcherNamespace

  -- Grab a fuller `-o wide` listing for the failure message; fall back to the raw
  -- watch line if that kubectl call fails.
  let snapshot fallback = handleAny (const (return fallback)) $
        readCreateProcessWithFileLogging ((proc kubectlBinary (["get", "pods"] <> nsArgs <> ["-o", "wide"])) { env = Just env }) ""

  let handleLine line = when ("OOMKilled" `L.isInfixOf` line) $ do
        existing <- readIORef oomRef
        when (isNothing existing) $ snapshot line >>= (writeIORef oomRef . Just)

  -- One streaming watch attempt: read one table row per pod update until it ends
  -- (EOF). A row whose STATUS is "OOMKilled" means a container was OOM-killed.
  let oneWatch = bracket
        (createProcess (proc kubectlBinary (["get", "pods"] <> nsArgs <> ["--watch", "--no-headers"])) { env = Just env, std_out = CreatePipe, std_err = CreatePipe, create_group = True })
        (liftIO . cleanupProcess)
        (\(_, mHOut, _, _) -> case mHOut of
            Nothing -> return ()
            Just hOut ->
              let loop = tryAny (liftIO (hGetLine hOut)) >>= \r -> case r of
                    Left _ -> return ()              -- EOF / closed: this watch ended
                    Right line -> handleLine line >> loop
              in loop)

  -- Watches drop periodically; re-establish until the action finishes (cancel).
  let watchForever = forever $ do
        handleAny (\e -> debug [i|(#{scope}) OOM pod-watch ended, re-establishing: #{e}|]) oneWatch
        threadDelay 1_000_000

  let finalCheck = do
        -- belt and suspenders: also do a one-shot check at teardown
        direct <- handleAny (const (return Nothing)) (findOOMKilled' kcc kubectlBinary oomWatcherNamespace)
        watched <- readIORef oomRef
        case (case watched of { Just d -> Just d; Nothing -> direct }) of
          Just detail -> expectationFailure [i|Detected an OOMKilled container in #{scope} during the run (memory/resource misconfiguration). Pods:\n#{detail}|]
          Nothing -> return ()

  finally (withAsync watchForever (const action)) finalCheck
