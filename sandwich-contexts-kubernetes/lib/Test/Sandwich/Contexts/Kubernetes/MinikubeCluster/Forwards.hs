{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinikubeCluster.Forwards where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import Network.URI
import Relude hiding (withFile)
import System.FilePath
import System.IO (hClose, hGetLine, openTempFile)
import System.Process (getPid)
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.ManagedAsync
import Test.Sandwich.Util.Process
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


withForwardKubernetesService' :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => KubernetesClusterContext -> Text -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService' (KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterMinikube {..}), ..}) profile namespace service action = do
  baseEnv <- liftIO getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  let extraFlags = case "--rootless" `L.elem` kubernetesClusterTypeMinikubeFlags of
        True -> ["--rootless"]
        False -> []

  let args = extraFlags <> [
        "--profile", toString profile
        , "--namespace", toString namespace
        , "--logtostderr"
        , "service"
        , toString service
        , "--url"]
  info [i|#{kubernetesClusterTypeMinikubeBinary} #{T.unwords $ fmap toText args}|]

  (stdoutRead, stdoutWrite) <- liftIO createPipe
  (stderrRead, stderrWrite) <- liftIO createPipe

  let forwardStderr =
        flip withException (\(e :: SomeException) -> debug [i|withForwardKubernetesService: stderr reader exited due to exception: #{e}|]) $
        forever $ do
          line <- liftIO $ hGetLine stderrRead
          info [i|minikube service stderr: #{line}|]

  managedWithAsync_ "" "minikube-service-stderr" forwardStderr $ do
    let cp = (proc kubernetesClusterTypeMinikubeBinary args) {
          env = Just env
          , std_out = UseHandle stdoutWrite
          , std_err = UseHandle stderrWrite
          , create_group = True
          }

    let stop (_, _, _, p) = liftIO (getPid p) >>= \case
          Nothing -> return ()
          Just _pid -> gracefullyStopProcess p 120_000_000

    bracket (createProcess cp) stop $ \_ -> do
      raw <- liftIO $ hGetLine stdoutRead

      info [i|withForwardKubernetesService': (#{namespace}) #{service} -> #{raw}|]

      action =<< case parseURI (toString (T.strip (toText raw))) of
        Nothing -> expectationFailure [i|Couldn't parse URI in withForwardKubernetesService': #{raw}|]
        Just x -> pure x
withForwardKubernetesService' _ _profile _namespace _service _action = error "Expected Minikube KubernetesClusterContext"

withForwardKubernetesServiceFileLogging' :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m, HasBaseContextMonad context m
  ) => KubernetesClusterContext -> Text -> Text -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesServiceFileLogging' (KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterMinikube {..}), ..}) fileName profile namespace service action = do
  baseEnv <- liftIO getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  let extraFlags = case "--rootless" `L.elem` kubernetesClusterTypeMinikubeFlags of
        True -> ["--rootless"]
        False -> []

  let args = extraFlags <> [
        "--profile", toString profile
        , "--namespace", toString namespace
        , "--logtostderr"
        , "service"
        , toString service
        , "--url"]
  info [i|#{kubernetesClusterTypeMinikubeBinary} #{T.unwords $ fmap toText args}|]

  let cp = (proc kubernetesClusterTypeMinikubeBinary args) {
        env = Just env
        , create_group = True
        }

  getCurrentFolder >>= \case
    Nothing -> expectationFailure [i|withForwardKubernetesServiceFileLogging': no current folder.|]
    Just dir ->
      bracket (liftIO $ openTempFile dir (toString fileName <.> "out")) (\(_outFile, hOut) -> liftIO $ hClose hOut) $ \(outFile, hOut) ->
      bracket (liftIO $ openTempFile dir (toString fileName <.> "err")) (\(_errFile, hErr) -> liftIO $ hClose hErr) $ \(_errFile, hErr) -> do
        let stop (_, _, _, p) = liftIO (getPid p) >>= \case
              Nothing -> return ()
              Just _pid -> gracefullyStopProcess p 120_000_000

        bracket (createProcess (cp { std_out = UseHandle hOut, std_err = UseHandle hErr })) stop $ \_ -> do
          raw <- readFirstLine outFile

          info [i|withForwardKubernetesServiceFileLogging': (#{namespace}) #{service} -> #{raw}|]

          action =<< case parseURI (toString (T.strip raw)) of
            Nothing -> expectationFailure [i|Couldn't parse URI in withForwardKubernetesServiceFileLogging': #{raw}|]
            Just x -> pure x
  where
    readFirstLine :: MonadIO m => FilePath -> m Text
    readFirstLine fp = liftIO loop
      where
        loop = do
          contents <- T.readFile fp
          case T.lines contents of
            (x:_) -> pure x
            [] -> threadDelay 50_000 >> loop
withForwardKubernetesServiceFileLogging' _ _fileName _profile _namespace _service _action = error "Expected Minikube KubernetesClusterContext"
