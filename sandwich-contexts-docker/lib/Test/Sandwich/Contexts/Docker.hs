
module Test.Sandwich.Contexts.Docker (
  DockerState(..)
  , createNetwork
  , doesNetworkExist
  , getDockerState

  -- , connectNetwork
  -- , disconnectNetwork
  -- , isConnectedToNetwork
  -- , joinContainerNetwork
  -- , leaveContainerNetwork
  -- , removeNetwork
  -- , waitForHealth
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IP
import qualified Data.Map as M
import Data.String.Interpolate
import DockerEngine.API.Network
import DockerEngine.Client
import DockerEngine.Core
import DockerEngine.MimeTypes
import DockerEngine.Model hiding (Map, Status(..))
import Network.HTTP.Client as NH
import Network.HTTP.Types.Status
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import Relude
import Test.Sandwich
import UnliftIO.Exception


data DockerState = DockerState {
  dockerEngineConfig :: DockerEngineConfig
  , dockerHttpManager :: Manager
  }

getDockerState :: (MonadLoggerIO m) => Bool -> m DockerState
getDockerState logToStdout = do
  config <- liftIO (newConfig >>= (if logToStdout then withStdoutLogging else return))
  manager <- liftIO $ newUnixDomainSocketManager "/var/run/docker.sock"
  return $ DockerState config manager
  where
    newUnixDomainSocketManager :: FilePath -> IO Manager
    newUnixDomainSocketManager path = do
      newManager $ defaultManagerSettings { managerRawConnection = return $ openUnixSocket path }
      where
        openUnixSocket filePath _ _ _ = do
          s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
          S.connect s (S.SockAddrUnix filePath)
          makeConnection (SBS.recv s 8096)
                         (SBS.sendAll s)
                         (S.close s)

doesNetworkExist :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => DockerState -> Text -> m Bool
doesNetworkExist ds networkName = isRight <$> inspectNetwork ds (Id networkName)

createNetwork :: (HasCallStack, MonadUnliftIO m, MonadLoggerIO m) => DockerState -> Text -> Map Text Text -> Either () (Maybe (AddrRange IPv4)) -> m (Either Text ())
createNetwork ds networkName labels ipv6OrSubnet = leftOnException $ do
  let networkConfig = (mkNetworkCreateRequest networkName) {
        networkCreateRequestAttachable = Just True
        , networkCreateRequestLabels = Just $ M.mapKeys toString labels
        , networkCreateRequestIpam = case ipv6OrSubnet of
            Right (Just subnet) -> Just $ mkIPAM {
              iPAMDriver = Just "default"
              -- ^ Need to set this default driver explicitly; see https://github.com/docker/compose/issues/5248
              , iPAMConfig = Just [mkIPAMConfig { iPAMConfigSubnet = Just (show subnet) }]
              }
            _ -> Nothing
        , networkCreateRequestEnableIpv6 = case ipv6OrSubnet of
            Left _ -> Just True
            _ -> Nothing
        }
  let req = networkCreate networkConfig
  runDockerEngineLBS ds req >>= \case
    (is2xx -> True) -> return $ Right ()
    x@(is403 -> True) -> return $ Left [i|Failed to create network '#{networkName}'. operation not supported for pre-defined networks: '#{x}'|]
    x@(is404 -> True) -> return $ Left [i|Failed to create network '#{networkName}'. Plugin not found: '#{x}'|]
    x@(is5xx -> True) -> return $ Left [i|Server error in createNetwork for '#{networkName}': '#{x}'|]
    x -> return $ Left [i|Unexpected response in createNetwork for '#{networkName}': '#{x}'|]

-- isConnectedToNetwork :: (
--   HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Id -> Text -> m (Either Text Bool)
-- isConnectedToNetwork ds containerName networkName = runExceptT $ do
--   joinedNetworks <- ExceptT $ getJoinedNetworks ds containerName
--   return (networkName `elem` joinedNetworks)

-- connectNetwork :: (
--   HasCallStack, MonadCatch m, MonadLoggerIO m, MonadUnliftIO m
--   ) => DockerState -> Text -> Text -> m (Either Text ())
-- connectNetwork ds containerName networkName = leftOnException $ do
--   let containers = NetworkConnectConfig (Just containerName) Nothing
--   let req = (networkConnect (Accept MimeJSON) containers (Id networkName)) :: DockerEngineRequest NetworkConnect MimeJSON () MimeJSON
--   runDockerEngineLBS ds req >>= \case
--     (is2xx -> True) -> return $ Right ()
--     (is403 -> True) -> return $ Left [i|Couldn't find network '#{networkName}' in connectNetwork|]
--     (is404 -> True) -> return $ Left [i|Couldn't find network '#{networkName}' in connectNetwork|]
--     x@(is5xx -> True) -> return $ Left [i|Server error in connectNetwork for '#{networkName}': '#{x}'|]
--     x -> return $ Left [i|Unexpected response in connectNetwork for '#{networkName}': '#{x}'|]

-- disconnectNetwork :: (
--   HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Text -> Text -> Bool -> m (Either Text ())
-- disconnectNetwork ds containerName networkName forceDisconnect = leftOnException $ do
--   let containers = NetworkDisconnectConfig (Just containerName) (Just forceDisconnect)
--   let req = networkDisconnect (Accept MimeJSON) containers (Id networkName) :: DockerEngineRequest NetworkDisconnect MimeJSON () MimeJSON
--   runDockerEngineLBS ds req >>= \case
--     (is2xx -> True) -> return $ Right ()
--     (is403 -> True) -> return $ Left [i|Forbidden response for '#{networkName}' in disconnectNetwork|]
--     (is404 -> True) -> return $ Left [i|Couldn't find network '#{networkName}' in disconnectNetwork|]
--     x@(is5xx -> True) -> return $ Left [i|Server error in disconnectNetwork for '#{networkName}': '#{x}'|]
--     x -> return $ Left [i|Unexpected response in disconnectNetwork for '#{networkName}': '#{x}'|]

-- removeNetwork :: (
--   HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Text -> m (Either Text ())
-- removeNetwork ds networkName = leftOnException $ do
--   let req = (networkDelete (Accept MimeJSON) (Id networkName)) :: DockerEngineRequest NetworkDelete MimeNoContent () MimeJSON
--   runDockerEngineLBS ds req >>= \case
--     (is2xx -> True) -> return $ Right ()
--     (is403 -> True) -> return $ Left [i|Forbidden to remove network '#{networkName}' in removeNetwork|]
--     (is404 -> True) -> return $ Left [i|Couldn't find network '#{networkName}' in removeNetwork|]
--     x@(is5xx -> True) -> return $ Left [i|Server error in removeNetwork for '#{networkName}': '#{x}'|]
--     x -> return $ Left [i|Unexpected response in removeNetwork for '#{networkName}': '#{x}'|]

-- joinContainerNetwork :: (
--   HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Text -> Text -> m (Either Text ())
-- joinContainerNetwork ds containerID networkName = leftOnException $ runExceptT $ do
--   currentlyJoinedNetworks :: [Text] <- ExceptT $ getJoinedNetworks ds (Id containerID)
--   case networkName `elem` currentlyJoinedNetworks of
--     True -> return ()
--     False -> ExceptT $ connectNetwork ds containerID networkName

-- leaveContainerNetwork :: (
--   HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Text -> Text -> m (Either Text ())
-- leaveContainerNetwork ds ownContainerID networkName = runExceptT $ do
--   currentlyJoinedNetworks :: [Text] <- ExceptT $ getJoinedNetworks ds (Id ownContainerID)
--   case networkName `elem` currentlyJoinedNetworks of
--     True -> ExceptT $ disconnectNetwork ds ownContainerID networkName False
--     False -> return ()

-- * Internal

runDockerEngineLBS :: (HasCallStack, Produces req accept, MimeType contentType, MonadLoggerIO m)
  => DockerState -> DockerEngineRequest req contentType res accept -> m (NH.Response BL8.ByteString)
runDockerEngineLBS ds req = do
  runDockerEngineLBS' ds req

runDockerEngineLBS' :: (HasCallStack, Produces req accept, MimeType contentType, MonadLoggerIO m)
  => DockerState -> DockerEngineRequest req contentType res accept -> m (NH.Response BL8.ByteString)
runDockerEngineLBS' (DockerState config manager) req = do
  debug [i|---> #{req}|]
  liftIO $ dispatchLbs manager config req

-- getJoinedNetworks :: (
--   MonadLoggerIO m, MonadUnliftIO m, MonadCatch m
--   ) => DockerState -> Id -> m (Either Text [Text])
-- getJoinedNetworks ds ident = runExceptT $ do
--   maybeInspectResponse <- ExceptT $ inspectContainer ds ident
--   case maybeInspectResponse of
--     Nothing -> throwE [i|Couldn't find container '#{ident}'|]
--     Just (ContainerInspectResponse {..}) -> do
--       let networkMap = fromMaybe mempty (containerInspectResponseNetworkSettings >>= networkSettingsNetworks)
--       return $ fmap toText $ M.keys networkMap

-- inspectContainer :: (
--   HasCallStack, MonadCatch m, MonadLoggerIO m, MonadUnliftIO m
--   ) => DockerState -> Id -> m (Either Text (Maybe ContainerInspectResponse))
-- -- Specifically check for an empty string, because trying to inspect that will lead to
-- -- effectively calling the /containers/{id}/json endpoint without the middle part, which
-- -- results in an array of inspect results for all containers and fails to parse.
-- inspectContainer _ (Id "") = return $ Right Nothing
-- inspectContainer ds ident = leftOnException $ runExceptT $ do
--   let req = containerInspect ident
--   lift (runDockerEngineLBS ds req) >>= \case
--     resp@(is2xx -> True) -> do
--       case A.eitherDecode (responseBody resp) of
--         Left err -> throwE [i|Failed to decode result in inspectContainer: '#{err}'|]
--         Right (x :: ContainerInspectResponse) -> return $ Just x
--     (is404 -> True) -> return Nothing
--     x@(is5xx -> True) -> throwE [i|Server error in inspectContainer for '#{ident}': '#{x}'|]
--     x -> throwE [i|Unexpected response in inspectContainer for '#{ident}': '#{x}'|]

inspectNetwork :: (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m
  ) => DockerState -> Id -> m (Either Text Network)
inspectNetwork (DockerState config manager) ident = leftOnException $ do
  let req = networkInspect ident
  debug [i|---> #{req}|]
  liftIO (dispatchMime manager config req) >>= \case
    MimeResult (Left err) _ -> return $ Left [i|(#{ident}) inspectNetwork failed: '#{err}'|]
    MimeResult (Right result) _ -> return $ Right result

-- waitForHealth :: forall m. (
--   HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadUnliftIO m, MonadBaseControl IO m, MonadThrow m
--   ) => Text -> m ()
-- waitForHealth containerID = race (liftIO $ threadDelay 60_000_000) waitForHealthLoop >>= \case
--   Left () -> do
--     info [i|Failed to wait for container #{containerID} to be ready|]
--     expectationFailure [i|Failed to wait for container to be ready|]
--   Right () -> return ()
--   where
--     waitForHealthLoop :: m ()
--     waitForHealthLoop = fix $ \loop -> do
--       health <- (strip . toText) <$> (liftIO $ readCreateProcess (shell [i|docker inspect --format "{{json .State.Health.Status }}" #{containerID}|]) "")
--       case health of
--        "\"healthy\"" -> return ()
--        _ -> liftIO (threadDelay 100_000) >> loop


-- * HTTP

is2xx :: NH.Response a -> Bool
is2xx (responseStatus -> (Status code _)) = code >= 200 && code < 300

-- is304 :: NH.Response a -> Bool
-- is304 (responseStatus -> (Status code _)) = code == 304

-- is400 :: NH.Response a -> Bool
-- is400 (responseStatus -> (Status code _)) = code == 400

is403 :: NH.Response a -> Bool
is403 (responseStatus -> (Status code _)) = code == 403

is404 :: NH.Response a -> Bool
is404 (responseStatus -> (Status code _)) = code == 404

-- is409 :: NH.Response a -> Bool
-- is409 (responseStatus -> (Status code _)) = code == 409

is5xx :: NH.Response a -> Bool
is5xx (responseStatus -> (Status code _)) = code >= 500 && code < 600

-- * Util

leftOnException :: (MonadUnliftIO m) => m (Either Text a) -> m (Either Text a)
leftOnException = handleAny $ \e -> return $ Left $ case fromException e of
  Just (Reason _ msg) -> toText msg
  _ -> show e
