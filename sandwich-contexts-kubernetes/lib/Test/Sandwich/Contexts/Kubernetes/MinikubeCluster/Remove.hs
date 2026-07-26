{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Remove (
  removeMinikubeCluster
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process


removeMinikubeCluster :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadReader context m, HasBaseContext context
  )
  => String
  -> FilePath
  -> Maybe Text
  -> [Text]
  -> [(String, String)]
  -> Bool
  -> FilePath
  -> m ()
removeMinikubeCluster clusterName minikubeBinary driver extraFlags' extraEnv forceRemove deleteLogFile = do
  -- For container-runtime drivers (docker/podman) each node is a container, with a
  -- volume named after it. Capture the names now, while the containers still exist and
  -- carry minikube's labels, so we can remove both below. VM-based drivers (kvm2, etc.)
  -- have neither, so we skip them.
  let containerRuntime = fromMaybe "docker" driver
  let usingContainerDriver = containerRuntime `L.elem` ["docker", "podman"]
  nodeNames <- if usingContainerDriver
    then getMinikubeClusterNodeNames containerRuntime clusterName
    else pure []

  if | forceRemove && containerRuntime == "docker" ->
         timeAction "force remove minikube containers" $
           removeClusterContainers containerRuntime nodeNames
     | otherwise ->
         timeAction "run minikube delete" $ do
           let extraFlags = case "--rootless" `L.elem` extraFlags' of
                 True -> ["--rootless"]
                 False -> []

           deleteEnv <- addOrReplaceEnv extraEnv <$> getEnvironment
           withFile deleteLogFile WriteMode $ \deleteH -> do
             let deleteCp = (proc minikubeBinary (["delete"
                                                  , "--profile", clusterName
                                                  , "--logtostderr"
                                                  ] <> extraFlags)) {
                   delegate_ctlc = True
                   , create_group = True
                   , env = Just deleteEnv
                   , std_out = UseHandle deleteH
                   , std_err = UseHandle deleteH
                   }
             withCreateProcess deleteCp $ \_ _ _ p ->
               waitForProcess p >>= \case
                 ExitSuccess -> return ()
                 ExitFailure n -> expectationFailure [i|Minikube cluster delete failed with code #{n}.|]

  timeAction "remove cluster volumes" $ deleteClusterVolumes containerRuntime nodeNames
  when usingContainerDriver $
    timeAction "remove cluster network" $ removeClusterNetwork containerRuntime clusterName
  timeAction "remove minikube profile dirs" $
    removeMinikubeProfileDirs clusterName extraEnv


-- | Resolve MINIKUBE_HOME the same way minikube does (see localpath.MiniPath): default to
-- @$HOME/.minikube@, honor a @MINIKUBE_HOME@ that already ends in @.minikube@, else append it.
-- A @MINIKUBE_HOME@ in the passed cluster-scoped env takes precedence over the ambient process env,
-- so profile cleanup targets the same home the cluster was actually started with.
getMinikubeHome :: MonadIO m => [(String, String)] -> m FilePath
getMinikubeHome extraEnv = resolve (L.lookup "MINIKUBE_HOME" extraEnv) >>= \case
  Just p -> return p
  Nothing -> UnliftIO.Environment.lookupEnv "MINIKUBE_HOME" >>= resolve >>= \case
    Just p -> return p
    Nothing -> (</> ".minikube") <$> getHomeDirectory
  where
    resolve :: MonadIO m => Maybe String -> m (Maybe FilePath)
    resolve = \case
      Just p | p /= "" -> return $ Just $ if takeFileName p == ".minikube" then p else p </> ".minikube"
      _ -> return Nothing

getMinikubeClusterNodeNames :: (MonadLoggerIO m, MonadUnliftIO m) => Text -> String -> m [String]
getMinikubeClusterNodeNames containerRuntime clusterName =
  handleAny (\e -> [] <$ warn [i|Couldn't list #{containerRuntime} containers for cluster #{clusterName}: #{e}|]) $
    readCreateProcessWithExitCode (proc (toString containerRuntime) [
        "ps", "-a"
        , "--filter", [i|label=mode.minikube.sigs.k8s.io=#{clusterName}|]
        , "--format", "{{.Names}}"
        ]) "" >>= \case
      (ExitSuccess, out, _) -> pure [toString l | l <- T.lines (toText out), not (T.null (T.strip l))]
      (ExitFailure _, _, _) -> pure []

-- | Force-remove the cluster's node containers, along with the anonymous volumes they own.
removeClusterContainers :: (MonadLoggerIO m) => Text -> [String] -> m ()
removeClusterContainers _ [] = return ()
removeClusterContainers containerRuntime names = forM_ names $ \name -> do
  debug [i|Removing #{containerRuntime} container: #{name}|]
  readCreateProcessWithExitCode (proc (toString containerRuntime) ["rm", "-f", "-v", name]) "" >>= \case
    (ExitSuccess, _, _) -> return ()
    (ExitFailure n, _, err) ->
      warn [i|Couldn't remove #{containerRuntime} container #{name} (exit #{n}); leaving it behind: #{err}|]

removeClusterNetwork :: (MonadLoggerIO m) => Text -> String -> m ()
removeClusterNetwork containerRuntime clusterName = do
  networks <- readCreateProcessWithExitCode cp "" >>= \case
    (ExitSuccess, out, _) ->
      return [toString l | l <- T.lines (toText out), not (T.null (T.strip l))]
    x -> expectationFailure [i|removeClusterNetwork: couldn't parse networks: #{x}|]

  forM_ networks  $ \net -> do
    debug [i|Removing #{containerRuntime} network: #{net}|]
    void $ readCreateProcessWithExitCode (proc (toString containerRuntime) ["network", "rm", net]) ""

  where
    cp = proc (toString containerRuntime) [
      "network", "ls"
      , "--filter", [i|label=name.minikube.sigs.k8s.io=#{clusterName}|]
      , "--format", "{{.Name}}"
      ]

-- | Force-remove the given container-runtime volumes ("--force" ignores any that are already gone,
-- and is supported by both @docker volume rm@ and @podman volume rm@). Best-effort.
deleteClusterVolumes :: (MonadLoggerIO m, MonadUnliftIO m) => Text -> [String] -> m ()
deleteClusterVolumes _ [] = return ()
deleteClusterVolumes containerRuntime volumes = handleAny logErr $ do
  debug [i|Removing leftover #{containerRuntime} volumes: #{volumes}|]
  void $ readCreateProcessWithExitCode (proc (toString containerRuntime) (["volume", "rm", "--force"] <> volumes)) ""
  where
    logErr :: MonadLoggerIO m => SomeException -> m ()
    logErr e = warn [i|Error removing #{containerRuntime} volumes #{volumes}: #{e}|]

-- | Remove the per-profile state that "minikube delete" should clean up but sometimes leaves behind:
-- @<minikube-home>/profiles/<profile>@ and @.../machines/<profile>@. The rest of MINIKUBE_HOME (the
-- multi-gigabyte image caches, certs, addons) is shared across clusters, so we leave it untouched.
removeMinikubeProfileDirs :: (MonadLoggerIO m, MonadUnliftIO m) => String -> [(String, String)] -> m ()
removeMinikubeProfileDirs clusterName extraEnv = handleAny logErr $ do
  miniPath <- getMinikubeHome extraEnv
  forM_ ["profiles", "machines"] $ \sub -> do
    let dir = miniPath </> sub </> clusterName
    whenM (doesPathExist dir) $ do
      debug [i|Removing leftover minikube state dir: #{dir}|]
      removePathForcibly dir
  where
    logErr :: MonadLoggerIO m => SomeException -> m ()
    logErr e = warn [i|Error removing minikube state dirs for #{clusterName}: #{e}|]
