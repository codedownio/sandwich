{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.PostgreSQL (
  postgres
  , introducePostgres
  , PostgresContext(..)

  -- * Raw
  , PostgresNixOptions(..)
  , defaultPostgresNixOptions
  , introducePostgresViaNix
  , withPostgresViaNix
  , introducePostgresUnixSocketViaNix
  , withPostgresUnixSocketViaNix

  -- * Containers
  , PostgresContainerOptions(..)
  , defaultPostgresContainerOptions
  , introducePostgresViaContainer
  , withPostgresContainer

  -- * Misc
  , NetworkAddress(..)
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (PortNumber)
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.ReverseProxy.TCP
import Test.Sandwich.Contexts.Util.Container
import Test.Sandwich.Contexts.Util.UUID
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IO (hClose, withFile)
import UnliftIO.Process

-- * Labels

postgres :: Label "postgres" PostgresContext
postgres = Label

-- * Types

data PostgresNixOptions = PostgresNixOptions {
  -- | Postgres version to use within the Nixpkgs snapshot of your 'NixContext'.
  -- Defaults to "postgresql", but you can pick specific versions like "postgresql_15".
  -- See "<nixpkgs>/top-level/all-packages.nix" for the available versions in your
  -- snapshot.
  postgresNixPostgres :: Text
  -- | Postgres username. Default to "postgres".
  , postgresNixUsername :: Text
  -- | Postgres password. Default to "postgres".
  , postgresNixPassword :: Text
  -- | Postgres default database. The "postgres" database is always created, but you
  -- can create an additional one here. Defaults to "test".
  , postgresNixDatabase :: Text
  }
defaultPostgresNixOptions :: PostgresNixOptions
defaultPostgresNixOptions = PostgresNixOptions {
  postgresNixPostgres = "postgresql"
  , postgresNixUsername = "postgres"
  , postgresNixPassword = "postgres"
  , postgresNixDatabase = "test"
  }

data NetworkAddress =
  NetworkAddressTCP { networkAddressTcpHostname :: String
                    , networkAddressTcpPort :: PortNumber }
  | NetworkAddressUnix { networkAddressUnixPath :: String }
  deriving (Show)

data PostgresContext = PostgresContext {
  postgresUsername :: Text
  , postgresPassword :: Text
  , postgresDatabase :: Text
  , postgresAddress :: NetworkAddress
  , postgresConnString :: Text
  -- | The address of the database server within its container (if it was started
  -- using a container).
  -- Useful when the test is also in a container, and containers are networked together.
  , postgresContainerAddress :: Maybe NetworkAddress
  } deriving (Show)


-- * Binary

-- initdb -D mydb
-- echo "listen_addresses=''" >> mydb/postgresql.conf
-- pg_ctl -D mydb -l logfile -o "--unix_socket_directories='$PWD'" start --wait

-- pg_ctl -D mydb -l logfile stop --wait

introducePostgres :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  ) => PostgresNixOptions -> SpecFree (LabelValue "postgres" PostgresContext :> context) m () -> SpecFree context m ()
introducePostgres opts@(PostgresNixOptions {..}) = introduceWith "PostgreSQL via Nix" postgres $ \action -> do
  withPostgresUnixSocketViaNix opts $ \unixSocket -> do
    debug [i|Got Postgres unix socket: #{unixSocket}|]
    void $ action $ PostgresContext {
      postgresUsername = postgresNixUsername
      , postgresPassword = postgresNixPostgres
      , postgresDatabase = postgresNixDatabase
      , postgresAddress = NetworkAddressUnix unixSocket
      , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@/#{postgresNixDatabase}?host=#{takeDirectory unixSocket}|]
      , postgresContainerAddress = Nothing
      }

introducePostgresViaNix :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  ) => PostgresNixOptions -> SpecFree (LabelValue "postgres" PostgresContext :> context) m () -> SpecFree context m ()
introducePostgresViaNix opts = introduceWith "PostgreSQL via Nix" postgres $ \action ->
  withPostgresViaNix opts (void . action)

withPostgresViaNix :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m, MonadFail m, MonadLogger m
  ) => PostgresNixOptions -> (PostgresContext -> m a) -> m a
withPostgresViaNix opts@(PostgresNixOptions {..}) action = do
  withPostgresUnixSocketViaNix opts $ \unixSocket ->
    withProxyToUnixSocket unixSocket $ \port ->
      action $ PostgresContext {
        postgresUsername = postgresNixUsername
        , postgresPassword = postgresNixPostgres
        , postgresDatabase = postgresNixDatabase
        , postgresAddress = NetworkAddressTCP "localhost" port
        , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@localhost:#{port}/#{postgresNixDatabase}|]
        , postgresContainerAddress = Nothing
        }

introducePostgresUnixSocketViaNix :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  ) => PostgresNixOptions -> SpecFree (LabelValue "postgres" PostgresContext :> context) m () -> SpecFree context m ()
introducePostgresUnixSocketViaNix opts@(PostgresNixOptions {..}) = introduceWith "PostgreSQL via Nix" postgres $ \action -> do
  withPostgresUnixSocketViaNix opts $ \unixSocket -> do
    void $ action $ PostgresContext {
      postgresUsername = postgresNixUsername
      , postgresPassword = postgresNixPostgres
      , postgresDatabase = postgresNixDatabase
      , postgresAddress = NetworkAddressUnix unixSocket
      , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@/#{postgresNixDatabase}?host=#{takeDirectory unixSocket}|]
      , postgresContainerAddress = Nothing
      }

withPostgresUnixSocketViaNix :: (
  MonadReader context m, HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadFail m, MonadMask m, MonadLogger m
  ) => PostgresNixOptions -> (FilePath -> m a) -> m a
withPostgresUnixSocketViaNix (PostgresNixOptions {..}) action = do
  nixEnv <- buildNixSymlinkJoin [postgresNixPostgres]

  Just dir <- getCurrentFolder
  baseDir <- liftIO $ createTempDirectory dir "postgres-nix"
  let dbDirName = baseDir </> "db"
  let logfileName = baseDir </> "logfile"

  -- The Unix socket can't live in the sandwich test tree because it has an absurdly short length
  -- requirement (107 bytes on Linux). See
  -- https://unix.stackexchange.com/questions/367008/why-is-socket-path-length-limited-to-a-hundred-chars
  withSystemTempDirectory "postgres-nix-unix-socks" $ \unixSockDir -> do
    bracket
      (do
          -- Run initdb
          baseEnv <- getEnvironment
          let env = ("LC_ALL", "C")
                  : ("LC_CTYPE", "C")
                  : baseEnv
          withTempFile baseDir "pwfile" $ \pwfile h -> do
            liftIO $ T.hPutStrLn h postgresNixPassword
            hClose h
            createProcessWithLogging ((proc (nixEnv </> "bin" </> "initdb") [dbDirName
                                                                            , "--username", toString postgresNixUsername
                                                                            , "-A", "md5"
                                                                            , "--pwfile", pwfile
                                                                            ]) {
                                         cwd = Just dir
                                         , env = Just env
                                         })
              >>= waitForProcess >>= (`shouldBe` ExitSuccess)

          -- Turn off the TCP interface; we'll have it listen solely on a Unix socket
          withFile (dir </> dbDirName </> "postgresql.conf") AppendMode $ \h -> liftIO $ do
            T.hPutStr h "\n"
            T.hPutStrLn h [i|listen_addresses=''|]

          -- Run pg_ctl to start the DB
          createProcessWithLogging ((proc (nixEnv </> "bin" </> "pg_ctl") [
                                        "-D", dbDirName
                                        , "-l", logfileName
                                        , "-o", [i|--unix_socket_directories='#{unixSockDir}'|]
                                        , "start" , "--wait"
                                        ]) { cwd = Just dir })
            >>= waitForProcess >>= (`shouldBe` ExitSuccess)

          -- Create the default db
          createProcessWithLogging ((proc (nixEnv </> "bin" </> "psql") [
                                        -- "-h", unixSockDir
                                        -- , "--username", toString postgresNixUsername
                                        [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@/?host=#{unixSockDir}|]
                                        , "-c", [i|CREATE DATABASE #{postgresNixDatabase};|]
                                        ]) { cwd = Just dir })
            >>= waitForProcess >>= (`shouldBe` ExitSuccess)


          files <- listDirectory (unixSockDir)
          filterM ((isSocket <$>) . liftIO . getFileStatus) [unixSockDir </> f | f <- files] >>= \case
            [f] -> pure f
            [] -> expectationFailure [i|Couldn't find Unix socket for PostgreSQL server (check output and logfile for errors).|]
            xs -> expectationFailure [i|Found multiple Unix sockets for PostgreSQL server, not sure which one to use: #{xs}|]
      )
      (\_ -> do
          void $ readCreateProcessWithLogging ((proc (nixEnv </> "bin" </> "pg_ctl") [
                                                   "-D", dbDirName
                                                   , "-l", logfileName
                                                   , "stop" , "--wait"
                                                   ]) { cwd = Just dir }) ""
      )
      (\socketPath -> action socketPath)

-- * Container

data PostgresContainerOptions = PostgresContainerOptions {
  postgresContainerUser :: Text
  , postgresContainerPassword :: Text
  , postgresContainerLabels :: Map Text Text
  , postgresContainerContainerName :: Maybe Text
  , postgresContainerContainerSystem :: ContainerSystem
  , postgresContainerImage :: Text
  } deriving (Show, Eq)
defaultPostgresContainerOptions :: PostgresContainerOptions
defaultPostgresContainerOptions = PostgresContainerOptions {
  postgresContainerUser = "postgres"
  , postgresContainerPassword = "password"
  , postgresContainerLabels = mempty
  , postgresContainerContainerName = Nothing
  , postgresContainerContainerSystem = ContainerSystemPodman
  , postgresContainerImage = "docker.io/postgres:15"
  }

introducePostgresViaContainer :: (
  HasBaseContext context
  , MonadUnliftIO m, MonadMask m
  ) => PostgresContainerOptions -> SpecFree (LabelValue "postgres" PostgresContext :> context) m () -> SpecFree context m ()
introducePostgresViaContainer opts = introduceWith "PostgreSQL via container" postgres $ \action -> do
  withPostgresContainer opts (void . action)

withPostgresContainer :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadMask m, MonadReader context m, HasBaseContext context
  ) => PostgresContainerOptions -> (PostgresContext -> m a) -> m a
withPostgresContainer options action = do
  bracket (createPostgresDatabase options)
          (\(containerName, _p) -> timeAction "cleanup Postgres database" $ do
              info [i|Doing #{postgresContainerContainerSystem options} rm -f --volumes #{containerName}|]
              (exitCode, sout, serr) <-  liftIO $ readCreateProcessWithExitCode (shell [i|#{postgresContainerContainerSystem options} rm -f --volumes #{containerName}|]) ""
              when (exitCode /= ExitSuccess) $
                expectationFailure [i|Failed to destroy Postgres container. Stdout: '#{sout}'. Stderr: '#{serr}'|]
          )
          (waitForPostgresDatabase options >=> action)

createPostgresDatabase :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, MonadReader context m, HasBaseContext context
  ) => PostgresContainerOptions -> m (Text, ProcessHandle)
createPostgresDatabase (PostgresContainerOptions {..}) = timeAction "create Postgres database" $ do
  containerName <- maybe (("postgres-" <>) <$> makeUUID) return postgresContainerContainerName

  let containerSystem = postgresContainerContainerSystem
  let labelArgs = mconcat [["-l", [i|#{k}=#{v}|]] | (k, v) <- M.toList postgresContainerLabels]
  let args = ["run"
             , "-d"
             , "-e", [i|POSTGRES_USER=#{postgresContainerUser}|]
             , "-e", [i|POSTGRES_PASSWORD=#{postgresContainerPassword}|]
             , "-p", "5432"
             , "--health-cmd", [i|pg_isready -U #{postgresContainerUser}|]
             , "--health-interval=100ms"
             , "--name", containerName
             ]
             <> labelArgs
             <> [postgresContainerImage]

  info [i|cmd: #{containerSystem} #{T.unwords args}|]

  p <- createProcessWithLogging (proc (show containerSystem) (fmap toString args))
  return (containerName, p)

waitForPostgresDatabase :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadMask m
  ) => PostgresContainerOptions -> (Text, ProcessHandle) -> m PostgresContext
waitForPostgresDatabase (PostgresContainerOptions {..}) (containerName, p) = do
  containerID <- waitForProcess p >>= \case
    ExitSuccess -> containerNameToContainerId postgresContainerContainerSystem containerName
    _ -> expectationFailure [i|Failed to start Postgres container.|]

  debug [i|Postgres container ID: #{containerID}|]

  localPort <- containerPortToHostPort postgresContainerContainerSystem containerName 5432

  waitForHealth postgresContainerContainerSystem containerID

  let pc = PostgresContext {
        postgresUsername = postgresContainerUser
        , postgresPassword = postgresContainerPassword
        , postgresDatabase = postgresContainerUser
        , postgresAddress = NetworkAddressTCP "localhost" localPort
        , postgresConnString = [i|postgresql://#{postgresContainerUser}:#{postgresContainerPassword}@localhost:#{localPort}/#{postgresContainerUser}|]
        , postgresContainerAddress = Just $ NetworkAddressTCP (toString containerName) 5432
        }

  -- TODO: might be a good idea to do this here, rather than wrap a retry around the initial migrate later on
  -- waitForSimpleQuery pc

  return pc
