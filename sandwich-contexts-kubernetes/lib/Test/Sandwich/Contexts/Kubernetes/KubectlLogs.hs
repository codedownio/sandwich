{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.Contexts.Kubernetes.KubectlLogs (
  withKubectlLogs
  , KubectlLogsContext (..)
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import Relude hiding (withFile)
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Util.Process (gracefullyStopProcess)
import UnliftIO.Exception
import UnliftIO.IO (withFile)
import UnliftIO.Process


-- * Types

data KubectlLogsContext = KubectlLogsContext {
  kubectlProcessHandle :: ProcessHandle
  }

-- * Implementation

-- | Run a @kubectl logs@ process, placing the logs in a file in the current test node directory.
--
-- Note that this will stop working if the pod you're talking to goes away (even if you do it against a service).
-- If this happens, a rerun of the command is needed to resume forwarding
withKubectlLogs :: (
  MonadLogger m, MonadFail m, MonadUnliftIO m
  , HasBaseContextMonad ctx m, HasFile ctx "kubectl"
  )
  -- | Kubeconfig file
  => FilePath
  -- | Namespace
  -> Text
  -- | Log target (pod, service, etc.)
  -> Text
  -- | Specific container to get logs from
  -> Maybe Text
  -- | Whether to interrupt the process to shut it down while cleaning up
  -> Bool
  -- | Callback receiving the 'KubectlLogsContext'
  -> (KubectlLogsContext -> m a)
  -> m a
withKubectlLogs kubeConfigFile namespace target maybeContainer interruptWhenDone action = do
  kubectlBinary <- askFile @"kubectl"

  let args = ["logs", toString target
             , "--namespace", toString namespace
             , "--kubeconfig", kubeConfigFile]
             <> (maybe [] (\x -> ["--container", toString x]) maybeContainer)

  Just dir <- getCurrentFolder
  let logPath = dir </> toString (T.replace "/" "_" target) <.> "log"

  debug [i|Running kubectl #{unwords $ fmap toText args} --> #{logPath}|]

  withFile logPath WriteMode $ \h -> do
    hSetBuffering h LineBuffering

    bracket (createProcess ((proc kubectlBinary args) {
                               std_out = UseHandle h
                               , std_err = UseHandle h
                               , create_group = True
                               }))
            (\(_, _, _, ps) -> if
                | interruptWhenDone -> void $ gracefullyStopProcess ps 30_000_000
                | otherwise -> void $ waitForProcess ps
            )
            (\(_, _, _, ps) -> do
                action $ KubectlLogsContext ps
            )
