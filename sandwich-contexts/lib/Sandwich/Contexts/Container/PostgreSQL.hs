{-# LANGUAGE GADTs #-}

module Sandwich.Contexts.Container.PostgreSQL (
  PostgresDatabaseTestContext (..)
  , withPostgresContainer

  , PostgresContextOptions (..)
  , defaultPostgresContextOptions

  , createPostgresDatabase
  , waitForPostgresDatabase
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Network.Socket (PortNumber)
import Relude
import Sandwich.Contexts.Util.Container
import Sandwich.Contexts.Util.UUID
import System.Exit
import Test.Sandwich
import UnliftIO.Exception
import UnliftIO.Process


-- * Types

data PostgresContextOptions = PostgresContextOptions {
  postgresContextUser :: Text
  , postgresContextPassword :: Text
  , postgresContextLabels :: Map Text Text
  , postgresContextContainerName :: Maybe Text
  , postgresContextContainerSystem :: ContainerSystem
  , postgresContextImage :: Text
  } deriving (Show, Eq)
defaultPostgresContextOptions :: PostgresContextOptions
defaultPostgresContextOptions = PostgresContextOptions {
  postgresContextUser = "postgres"
  , postgresContextPassword = "password"
  , postgresContextLabels = mempty
  , postgresContextContainerName = Nothing
  , postgresContextContainerSystem = ContainerSystemPodman
  , postgresContextImage = "docker.io/postgres:15"
  }

data PostgresDatabaseTestContext = PostgresDatabaseTestContext {
  postgresDatabaseLocalHostname :: Text
  , postgresDatabaseLocalPort :: PortNumber
  , postgresDatabaseUsername :: Text
  , postgresDatabasePassword :: Text
  , postgresDatabaseDatabase :: Text
  , postgresDatabaseContainerPort :: PortNumber
  , postgresDatabaseContainerName :: Text
  } deriving (Show, Eq)

-- * Functions

withPostgresContainer :: (
  HasCallStack, MonadUnliftIO m, MonadBaseControl IO m, MonadLoggerIO m, MonadMask m, MonadReader context m, HasBaseContext context
  ) => PostgresContextOptions -> (PostgresDatabaseTestContext -> m a) -> m a
withPostgresContainer options action = do
  bracket (createPostgresDatabase options)
          (\(containerName, _p) -> timeAction "cleanup Postgres database" $ do
              info [i|Doing #{postgresContextContainerSystem options} rm -f --volumes #{containerName}|]
              (exitCode, sout, serr) <-  liftIO $ readCreateProcessWithExitCode (shell [i|#{postgresContextContainerSystem options} rm -f --volumes #{containerName}|]) ""
              when (exitCode /= ExitSuccess) $
                expectationFailure [i|Failed to destroy Postgres container. Stdout: '#{sout}'. Stderr: '#{serr}'|]
          )
          (waitForPostgresDatabase options >=> action)

createPostgresDatabase :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m, HasBaseContext context
  ) => PostgresContextOptions -> m (Text, ProcessHandle)
createPostgresDatabase (PostgresContextOptions {..}) = timeAction "create Postgres database" $ do
  containerName <- maybe (("postgres-" <>) <$> makeUUID) return postgresContextContainerName

  let containerSystem = postgresContextContainerSystem
  let labelArgs = mconcat [["-l", [i|#{k}=#{v}|]] | (k, v) <- M.toList postgresContextLabels]
  let args = ["run"
             , "-d"
             , "-e", [i|POSTGRES_USER=#{postgresContextUser}|]
             , "-e", [i|POSTGRES_PASSWORD=#{postgresContextPassword}|]
             , "-p", "5432"
             , "--health-cmd", [i|pg_isready -U #{postgresContextUser}|]
             , "--health-interval=100ms"
             , "--name", containerName
             ]
             <> labelArgs
             <> [postgresContextImage]

  info [i|cmd: #{containerSystem} #{T.unwords args}|]

  p <- createProcessWithLogging (proc (show containerSystem) (fmap toString args))
  return (containerName, p)

waitForPostgresDatabase :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadMask m
  ) => PostgresContextOptions -> (Text, ProcessHandle) -> m PostgresDatabaseTestContext
waitForPostgresDatabase (PostgresContextOptions {..}) (containerName, p) = do
  containerID <- waitForProcess p >>= \case
    ExitSuccess -> containerNameToContainerId postgresContextContainerSystem containerName
    _ -> expectationFailure [i|Failed to start Postgres container.|]

  debug [i|Postgres container ID: #{containerID}|]

  localPort <- containerPortToHostPort postgresContextContainerSystem containerName 5432

  waitForHealth postgresContextContainerSystem containerID

  let pdtc = PostgresDatabaseTestContext {
        postgresDatabaseLocalHostname = "127.0.0.1"
        , postgresDatabaseLocalPort = localPort
        , postgresDatabaseUsername = postgresContextUser
        , postgresDatabasePassword = postgresContextPassword
        , postgresDatabaseDatabase = postgresContextUser
        , postgresDatabaseContainerPort = 5432
        , postgresDatabaseContainerName = containerName
        }

  -- TODO: might be a good idea to do this here, rather than wrap a retry around the initial migrate later on
  -- waitForSimpleQuery pdtc

  return pdtc
