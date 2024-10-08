{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.Contexts.Kubernetes.KubectlPortForward (
  withKubectlPortForward
  , withKubectlPortForward'
  , KubectlPortForwardContext (..)
  ) where

import Control.Monad
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Retry
import Data.String.Interpolate
import qualified Data.Text as T
import Network.Socket (PortNumber)
import Relude hiding (withFile)
import System.FilePath
import System.Process (getPid)
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Util.Ports
import Test.Sandwich.Contexts.Kubernetes.Util.SocketUtil
import Test.Sandwich.Util.Process (gracefullyStopProcess)
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.Process


-- * Types

newtype KubectlPortForwardContext = KubectlPortForwardContext {
  kubectlPortForwardPort :: PortNumber
  }

-- * Implementation

-- | Run a @kubectl port-forward@ process, making the port available in the 'KubectlPortForwardContext'.
--
-- Note that this will stop working if the pod you're talking to goes away (even if you do it against a service).
-- If this happens, a rerun of the command is needed to resume forwarding
withKubectlPortForward :: (
  HasCallStack, MonadCatch m, MonadLogger m, MonadUnliftIO m
  , HasBaseContextMonad ctx m, HasFile ctx "kubectl"
  )
  => FilePath
  -> Text
  -> Text
  -> PortNumber
  -> (KubectlPortForwardContext -> m a)
  -> m a
withKubectlPortForward kubeConfigFile namespace targetName targetPort action = do
  kubectlBinary <- askFile @"kubectl"
  withKubectlPortForward' kubectlBinary kubeConfigFile namespace (const True) Nothing targetName targetPort action

-- | Same as 'withKubectlPortForward', but allows you to pass in the @kubectl@ binary path.
withKubectlPortForward' :: (
  HasCallStack, MonadCatch m, MonadLogger m, MonadUnliftIO m
  , HasBaseContextMonad ctx m
  )
  => FilePath
  -> FilePath
  -> Text
  -> (PortNumber -> Bool)
  -> Maybe PortNumber
  -> Text
  -> PortNumber
  -> (KubectlPortForwardContext -> m a)
  -> m a
withKubectlPortForward' kubectlBinary kubeConfigFile namespace isAcceptablePort maybeHostPort targetName targetPort action = do
  port <- maybe (findFreePortOrException' isAcceptablePort) return maybeHostPort

  let args = ["port-forward", toString targetName, [i|#{port}:#{targetPort}|]
             , "--namespace", toString namespace
             , "--kubeconfig", kubeConfigFile]

  debug [i|Running kubectl #{unwords $ fmap toText args}|]

  dir <- getCurrentFolder >>= \case
    Just x -> pure (x </> "port-forwarding-logs-kubectl")
    Nothing -> expectationFailure [i|Expected a current folder in withKubectlPortForward'|]

  createDirectoryIfMissing True dir
  let logPath = dir </> toString (T.replace "/" "_" targetName) <.> "port-forwarding.log"

  withFile logPath WriteMode $ \h -> do

    let restarterThread = forever $ do
          bracket (createProcess ((proc kubectlBinary args) {
                                     std_out = UseHandle h
                                     , std_err = UseHandle h
                                     , create_group = True
                                     }))
                  (\(_, _, _, ps) -> gracefullyStopProcess ps 30000000)
                  (\(_, _, _, ps) -> do
                      pid <- liftIO $ getPid ps
                      info [i|Got pid for kubectl port forward: #{pid}|]

                      code <- waitForProcess ps
                      warn [i|kubectl port-forward #{targetName} #{port}:#{targetPort} exited with code: #{code}. Restarting...|]
                  )

    withAsync restarterThread $ \_ -> do
      let policy = constantDelay 100000 <> limitRetries 100
      void $ liftIO $ retrying policy (\_ ret -> return ret) $ \_ -> do
        not <$> isPortOpen (simpleSockAddr (127, 0, 0, 1) port)

      action $ KubectlPortForwardContext { kubectlPortForwardPort = port }
