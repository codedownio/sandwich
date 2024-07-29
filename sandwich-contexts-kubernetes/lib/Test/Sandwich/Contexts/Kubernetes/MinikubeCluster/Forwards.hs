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
import Network.URI
import Relude hiding (withFile)
import System.IO (hGetLine)
import System.Process (getPid)
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Util.Process
import UnliftIO.Async
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


withForwardKubernetesService' :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => KubernetesClusterContext -> Text -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService' (KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterMinikube {..}), ..}) profile namespace service action = do
  baseEnv <- liftIO getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  let extraFlags = case "--rootless" `L.elem` minikubeFlags of
        True -> ["--rootless"]
        False -> []

  let args = extraFlags <> [
        "--profile", toString profile
        , "--namespace", toString namespace
        , "--logtostderr"
        , "service"
        , toString service
        , "--url"]
  info [i|#{minikubeBinary} #{T.unwords $ fmap toText args}|]

  (stdoutRead, stdoutWrite) <- liftIO createPipe
  (stderrRead, stderrWrite) <- liftIO createPipe

  let forwardStderr = forever $ do
        line <- liftIO $ hGetLine stderrRead
        info [i|minikube service stderr: #{line}|]

  withAsync forwardStderr $ \_ -> do
    let cp = (proc minikubeBinary args) {
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
