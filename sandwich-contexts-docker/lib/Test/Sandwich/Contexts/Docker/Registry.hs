{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Docker.Registry (
  DockerRegistryContext (..)
  , introduceDockerRegistry
  , pushDockerImages
  , withDockerRegistry
  , withNewDockerRegistry

  , pushContainerToRegistryTimed
  , pushContainerToRegistry
  , parseDockerImage

  , dockerRegistry
  , HasDockerRegistryContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Network.HostName
import Network.Socket (PortNumber)
import Relude
import Safe
import qualified System.Random as R
import Test.Sandwich
import Test.Sandwich.Contexts.Docker (createNetwork, doesNetworkExist, getDockerState)
import Test.Sandwich.Contexts.Docker.Container (isInContainer)
import UnliftIO.Exception
import UnliftIO.Process


-- * Types

data DockerRegistryContext = DockerRegistryContext {
  dockerRegistryContainerName :: Text
  , dockerRegistryPort :: PortNumber
  } deriving (Show, Eq)

dockerRegistry :: Label "dockerRegistry" DockerRegistryContext
dockerRegistry = Label
type HasDockerRegistryContext context = HasLabel context "dockerRegistry" DockerRegistryContext

-- * Introduce

introduceDockerRegistry :: (
  HasCallStack, MonadUnliftIO m
  ) => SpecFree (LabelValue "dockerRegistry" DockerRegistryContext :> context) m () -> SpecFree context m ()
introduceDockerRegistry = introduceWith "introduce Docker registry" dockerRegistry $ \action -> do
  void $ withDockerRegistry Nothing action

pushDockerImages :: (
  HasCallStack, MonadUnliftIO m, HasDockerRegistryContext context, HasBaseContext context
  ) => [Text] -> SpecFree context m () -> SpecFree context m ()
pushDockerImages images = before "push Docker images" $ do
  drc <- getContext dockerRegistry
  forM_ images $ flip pushContainerToRegistryTimed drc

-- * Implementation

withDockerRegistry :: (
  MonadUnliftIO m, MonadLoggerIO m
  ) => Maybe (HostName, PortNumber) -> (DockerRegistryContext -> m a) -> m a
withDockerRegistry optExternalDockerRegistry action = do
  case optExternalDockerRegistry of
    Just (hostname, port) ->
      action $ DockerRegistryContext { dockerRegistryContainerName = toText hostname
                                     , dockerRegistryPort = port }
    Nothing -> withNewDockerRegistry action

withNewDockerRegistry :: (MonadUnliftIO m, MonadLoggerIO m) => (DockerRegistryContext -> m a) -> m a
withNewDockerRegistry action = do
  registryID <- makeUUID' 5

  let containerName = [i|test-registry-#{registryID}|]

  bracket (do
              -- Create the "kind" Docker bridge network first in case it doesn't exist
              ds <- getDockerState False
              networkExists <- doesNetworkExist ds "kind"
              unless networkExists $
                createNetwork ds "kind" mempty (Right Nothing) >>= \case
                  Right _ -> return ()
                  Left err -> warn [i|Creating Docker network "kind" failed: '#{err}'|]

              ps <- createProcessWithLogging (proc "docker" ["run", "-d", "--restart=always"
                                                            , "-p", [i|5000|]
                                                            ,  "--name", containerName
                                                            ,  "--net=kind"
                                                            , "registry:2"])
              waitForProcess ps
          )
          (\_ -> do
              info [i|Deleting registry '#{containerName}'|]
              void $ liftIO $ readCreateProcess (shell [i|docker rm -f --volumes #{containerName}|]) ""
          )
          (\_ -> do
              let inspectCommand = [i|docker inspect --format='{{index .NetworkSettings.Ports "5000/tcp" 0 "HostPort"}}' #{containerName}|]
              rawPort <- (strip . toText) <$> (liftIO $ readCreateProcess (shell inspectCommand) "")

              case readMay $ toString rawPort of
                Nothing -> expectationFailure [i|Couldn't read Docker port number: '#{rawPort}'|]
                Just port ->
                  action $ DockerRegistryContext {
                    dockerRegistryContainerName = toText containerName
                    , dockerRegistryPort = port
                    }
          )

-- * Util

pushContainerToRegistryTimed :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  ) => Text -> DockerRegistryContext -> m Text
pushContainerToRegistryTimed imageName drc = timeAction [i|Pushing docker image '#{imageName}'|] $
  pushContainerToRegistry imageName drc

pushContainerToRegistry :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m
  ) => Text -> DockerRegistryContext -> m Text
pushContainerToRegistry imageName (DockerRegistryContext {..}) = do
  imageNamePart <- case splitOn "/" imageName of
    [_, y] -> return y
    _ -> expectationFailure [i|Unexpected format for image name: '#{imageName}'|]

  let pushedName :: Text = [i|localhost:#{dockerRegistryPort}/#{imageNamePart}|]
  void $ liftIO $ readCreateProcess (shell [i|docker tag #{imageName} #{pushedName}|]) ""

  debug [i|Pushing docker image '#{pushedName}'...|]
  liftIO isInContainer >>= \case
    True -> do
      -- We need to push to our local registry, but we'll get an insecure Docker registry error unless
      -- we're pushing to localhost. To accomplish this, we'll launch a new Docker container with host networking
      -- to do the push
      ps <- createProcessWithLogging (shell [i|docker run --rm --network host -v /var/run/docker.sock:/var/run/docker.sock docker:stable docker push #{pushedName}|])
      void $ liftIO $ waitForProcess ps

    False -> do
      ps <- createProcessWithLogging (shell [i|docker push #{pushedName}|])
      void $ liftIO $ waitForProcess ps

  debug [i|finished pushing.|]

  let (_repository, baseName, imageTag) = parseDockerImage imageName
  return [i|#{dockerRegistryContainerName}:5000/#{baseName}#{imageTag}|]

parseDockerImage :: (HasCallStack) => Text -> (Text, Text, Text)
parseDockerImage image = case splitOn "/" image of
  [repo, imageName] -> case splitOn ":" imageName of
    [name] -> (repo, name, "")
    [name, tag] -> (repo, name, ":" <> tag)
    _ -> error [i|Expected image name part to have zero or one colon, but it was '#{imageName}'|]
  [host, repo, imageName] -> case splitOn ":" imageName of
    [name] -> (repo, name, "")
    [name, tag] -> (host <> "/" <> repo, name, ":" <> tag)
    _ -> error [i|Expected image name part to have zero or one colon, but it was '#{imageName}'|]
  _ -> error [i|Expected image to contain a single slash, but it was '#{image}'|]


-- | For testing this interactively
-- main :: IO ()
-- main = do
--   runStdoutLoggingT $ withDockerRegistry $ \(DockerRegistryContext {..}) -> do
--     liftIO $ putStrLn [i|Docker registry '#{dockerRegistryContainerName}' started on port #{dockerRegistryPort}! Sleeping 1 minute|]
--     liftIO $ sleep 60000000

-- * Util

makeUUID' :: MonadIO m => Int -> m T.Text
makeUUID' n = toText <$> (replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1)))
  where
    -- Note: for a UUID to appear in a Kubernetes name, it needs to match this regex
    -- [a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*'
    uuidLetters :: [Char]
    uuidLetters = ['a'..'z'] ++ ['0'..'9']

    numUUIDLetters :: Int
    numUUIDLetters = L.length uuidLetters
