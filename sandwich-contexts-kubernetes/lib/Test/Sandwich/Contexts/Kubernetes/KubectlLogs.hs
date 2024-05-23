
module Test.Sandwich.Contexts.Kubernetes.KubectlLogs (
  KubectlLogsContext (..)
  , withKubectlLogs
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import Relude hiding (withFile)
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Util.Process (gracefullyStopProcess)
import UnliftIO.Exception
import UnliftIO.IO (withFile)
import UnliftIO.Process


-- * Types

data KubectlLogsContext = KubectlLogsContext

-- * Implementation

-- | Note that this will stop working if the pod you're talking to goes away (even if you do it against a service)
-- If this happens, a rerun of the command is needed to resume forwarding
withKubectlLogs :: (
  HasBaseContextMonad ctx m, MonadLogger m, MonadFail m, MonadUnliftIO m
  ) => FilePath -> Text -> Text -> Maybe Text -> Bool -> (KubectlLogsContext -> m a) -> m a
withKubectlLogs kubeConfigFile namespace target maybeContainer interruptWhenDone action = do
  let args = ["logs", toString target
             , "--namespace", toString namespace
             , "--kubeconfig", kubeConfigFile]
             <> (maybe [] (\x -> ["--container", toString x]) maybeContainer)

  Just dir <- getCurrentFolder
  let logPath = dir </> toString (T.replace "/" "_" target) <.> "log"

  debug [i|Running kubectl #{unwords $ fmap toText args} --> #{logPath}|]

  withFile logPath WriteMode $ \h -> do
    hSetBuffering h LineBuffering

    bracket (createProcess ((proc "kubectl" args) { std_out = UseHandle h
                                                  , std_err = UseHandle h
                                                  , create_group = True
                                                  }))
            (\(_, _, _, ps) -> if
                | interruptWhenDone -> void $ gracefullyStopProcess ps 30_000_000
                | otherwise -> void $ waitForProcess ps
            )
            (\_ -> do
                action KubectlLogsContext
            )
