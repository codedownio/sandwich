{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Sandwich.Contexts.Util.Container (
  ContainerSystem (..)

  , isInContainer

  , containerPortToHostPort

  , containerNameToContainerId

  , readUncompressedImageName

  , waitForHealth
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Retry
import Data.Aeson as A
import Data.Aeson.TH as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Socket (PortNumber)
import Relude
import Safe
import Sandwich.Contexts.Util.Aeson
import System.Exit
import System.FilePath
import Test.Sandwich
import qualified Text.Show
import UnliftIO.Process


data ContainerSystem = ContainerSystemDocker | ContainerSystemPodman
  deriving (Eq)

instance Show ContainerSystem where
  show ContainerSystemDocker = "docker"
  show ContainerSystemPodman = "podman"

isInContainer :: MonadIO m => m Bool
isInContainer = do
  output <- toText <$> readCreateProcess (shell "cat /proc/1/sched | head -n 1") ""
  return $ not $
    ("init" `T.isInfixOf` output)
    || ("systemd" `T.isInfixOf` output)
    || ("bwrap" `T.isInfixOf` output)

waitForHealth :: forall m. (HasCallStack, MonadLoggerIO m, MonadMask m) => ContainerSystem -> Text -> m ()
waitForHealth containerSystem containerID = do
  let policy = limitRetriesByCumulativeDelay (60 * 1_000_000) $ capDelay 1_000_000 $ exponentialBackoff 1000
  recoverAll policy $ \_ -> do
    health <- (T.strip . toText) <$> (readCreateProcess (
      shell [i|#{containerSystem} inspect --format "{{json .State.Health.Status }}" #{containerID}|]) ""
      )

    case health of
      "\"healthy\"" -> return ()
      _ -> do
        -- Try running the health check manually, when possible.
        -- This is a workaround for rootless podman failing to have working healthchecks.
        when (containerSystem == ContainerSystemPodman) $ do
          -- TODO: use createProcessWithLogging here?
          (exitCode, sout, serr) <- readCreateProcessWithExitCode (proc "podman" ["healthcheck", "run", toString containerID]) ""
          when (exitCode /= ExitSuccess) $ do
            warn [i|Failed to manually run healthcheck. Code: #{exitCode}. Stdout: '#{sout}'. Stderr: '#{serr}'.|]

        expectationFailure [i|Health was: #{health}|]


data HostPortInfo = HostPortInfo {
  hostPortInfoHostIp :: Text
  , hostPortInfoHostPort :: Text
  }
deriveJSON (A.defaultOptions { A.fieldLabelModifier = L.drop (L.length ("hostPortInfo" :: String)) }) ''HostPortInfo

containerPortToHostPort :: (HasCallStack, MonadIO m) => ContainerSystem -> Text -> PortNumber -> m PortNumber
containerPortToHostPort containerSystem containerName containerPort = do
  let inspectPortCmd = [i|#{containerSystem} inspect --format='{{json .NetworkSettings.Ports}}' #{containerName}|]

  rawNetworkSettings <- liftIO (readCreateProcessWithExitCode (shell inspectPortCmd) "") >>= \case
    (ExitSuccess, sout, _serr) -> return $ T.strip $ toText sout
    (ExitFailure n, sout, serr) -> expectationFailure [i|Failed to read container ports (error code #{n}). Stdout: '#{sout}'. Stderr: '#{serr}'.|]

  networkSettings :: Map Text [HostPortInfo] <- case A.eitherDecode (encodeUtf8 rawNetworkSettings) of
    Left err -> expectationFailure [i|Failed to decode network settings: #{err}. Settings were #{rawNetworkSettings}.|]
    Right x -> pure x

  rawPort <- case M.lookup [i|#{containerPort}/tcp|] networkSettings of
    Just (x:_) -> pure $ hostPortInfoHostPort x
    _ -> expectationFailure [i|Couldn't find any host ports corresponding to container port #{containerPort}. Network settings: #{A.encode networkSettings}|]

  case readMay (toString rawPort) of
    Just x -> pure x
    Nothing -> expectationFailure [i|Couldn't read container port number: '#{rawPort}'|]

containerNameToContainerId :: (HasCallStack, MonadIO m) => ContainerSystem -> Text -> m Text
containerNameToContainerId containerSystem containerName = do
  let cmd = [i|#{containerSystem} inspect --format='{{.Id}}' #{containerName}|]
  liftIO (readCreateProcessWithExitCode (shell cmd) "") >>= \case
    (ExitSuccess, sout, _serr) -> return $ T.strip $ toText sout
    (ExitFailure n, sout, serr) -> expectationFailure [i|Failed to obtain container ID for container named '#{containerName}'. Code: #{n}. Stdout: '#{sout}'. Stderr: '#{serr}'.|]

readUncompressedImageName :: (HasCallStack, MonadIO m) => FilePath -> m Text
readUncompressedImageName path = do
  contents <- liftIO $ BL.readFile (path </> "manifest.json")

  case A.eitherDecode contents of
    Left err -> expectationFailure [i|Couldn't decode manifest.json: #{err}|]
    Right (A.Array entries) -> case concatMap getRepoTags entries of
      (x:_) -> pure x
      [] -> expectationFailure [i|Didn't find a repo tag for image at #{path}|]
    Right x -> expectationFailure [i|Unexpected manifest.json format: #{x}|]

  where
    getRepoTags :: A.Value -> [Text]
    getRepoTags (A.Object (aesonLookup "RepoTags" -> Just (A.Array repoItems))) = [t | A.String t <- V.toList repoItems]
    getRepoTags _ = []
