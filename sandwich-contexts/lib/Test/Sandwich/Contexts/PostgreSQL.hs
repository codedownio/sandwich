{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|

This module provides tools for introducing PostgreSQL databases, either via a container (Docker or Podman) or
via a raw process (typically obtaining the binary from Nix).

The container method is traditional, but the raw method can be nice because it tends to leave less junk on the
system such as container images, networks, and volumes.

A note about raw processes and random TCP ports: starting a Postgres process on a randomly chosen port is tricky,
because Postgres currently lacks a setting for choosing its own port and reporting it back to us. So, the only way
to start it on a random TCP port is to first manually  find a free port on the system and then start Postgres
with it. Since this procedure is inherently racy, it can cause failures if your tests are starting lots of
Postgres instances (or other network-using processes) in parallel. This module takes a different approach: it starts
the Postgres instance on a Unix socket, which can never fail. You can connect to it via the Unix socket directly if
you like. If you use the TCP-based methods like 'introducePostgresViaNix', they will open a TCP socket inside the
test process and then run a proxy to forward packets to the Postgres server's Unix socket.

-}

module Test.Sandwich.Contexts.PostgreSQL (
#ifndef mingw32_HOST_OS
  -- * Raw PostgreSQL via Nix (TCP socket)
  introducePostgresViaNix
  , withPostgresViaNix

  -- * Raw PostgreSQL via Nix (Unix socket)
  , introducePostgresUnixSocketViaNix
  , withPostgresUnixSocketViaNix

  -- * Containerized PostgreSQL
  , introducePostgresViaContainer
  , withPostgresContainer

  -- * Types
  , PostgresNixOptions(..)
  , defaultPostgresNixOptions

  , postgres
  , PostgresContext(..)

  , PostgresContainerOptions(..)
  , defaultPostgresContainerOptions

  -- * Re-exports
  , NetworkAddress(..)
#endif
  ) where

#ifndef mingw32_HOST_OS
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import System.IO.Temp
import System.PosixCompat.Files (getFileStatus, isSocket)
import Test.Sandwich
import Test.Sandwich.Contexts.Container
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.ReverseProxy.TCP
import Test.Sandwich.Contexts.Types.Network
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
  -- Defaults to "postgresql", but you can pick specific versions like @postgresql_15@.
  -- See @\<nixpkgs\>\/top-level\/all-packages.nix@ for the available versions in your
  -- snapshot.
  postgresNixPostgres :: Text
  -- | Postgres username. Default to @postgres@.
  , postgresNixUsername :: Text
  -- | Postgres password. Default to @postgres@.
  , postgresNixPassword :: Text
  -- | Postgres default database. The @postgres@ database is always created, but you
  -- can create an additional one here. Defaults to @test@.
  , postgresNixDatabase :: Text
  }
defaultPostgresNixOptions :: PostgresNixOptions
defaultPostgresNixOptions = PostgresNixOptions {
  postgresNixPostgres = "postgresql"
  , postgresNixUsername = "postgres"
  , postgresNixPassword = "postgres"
  , postgresNixDatabase = "test"
  }

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

-- | Introduce a PostgreSQL instance, using a suitable package from Nix.
introducePostgresViaNix :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  )
  -- | Options
  => PostgresNixOptions
  -> SpecFree (LabelValue "postgres" PostgresContext :> context) m ()
  -> SpecFree context m ()
introducePostgresViaNix opts = introduceWith "PostgreSQL via Nix" postgres $ \action ->
  withPostgresViaNix opts (void . action)

-- | Bracket-style variant of 'introducePostgresViaNix'.
withPostgresViaNix :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadMask m, MonadFail m, MonadLogger m
  )
  -- | Options
  => PostgresNixOptions
  -> (PostgresContext -> m a)
  -> m a
withPostgresViaNix opts@(PostgresNixOptions {..}) action = do
  withPostgresUnixSocketViaNix opts $ \unixSocket ->
    withProxyToUnixSocket unixSocket $ \port ->
      action $ PostgresContext {
        postgresUsername = postgresNixUsername
        , postgresPassword = postgresNixPassword
        , postgresDatabase = postgresNixDatabase
        , postgresAddress = NetworkAddressTCP "localhost" port
        , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@localhost:#{port}/#{postgresNixDatabase}|]
        , postgresContainerAddress = Nothing
        }

-- | Same as 'introducePostgresViaNix', but the 'postgresAddress' of the 'PostgresContext' will be a Unix socket.
introducePostgresUnixSocketViaNix :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  )
  -- | Options
  => PostgresNixOptions
  -> SpecFree (LabelValue "postgres" PostgresContext :> context) m ()
  -> SpecFree context m ()
introducePostgresUnixSocketViaNix opts@(PostgresNixOptions {..}) = introduceWith "PostgreSQL via Nix" postgres $ \action -> do
  withPostgresUnixSocketViaNix opts $ \unixSocket -> do
    void $ action $ PostgresContext {
      postgresUsername = postgresNixUsername
      , postgresPassword = postgresNixPassword
      , postgresDatabase = postgresNixDatabase
      , postgresAddress = NetworkAddressUnix unixSocket
      , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@/#{postgresNixDatabase}?host=#{takeDirectory unixSocket}|]
      , postgresContainerAddress = Nothing
      }

-- | Bracket-style variant of 'introducePostgresUnixSocketViaNix'.
withPostgresUnixSocketViaNix :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadFail m, MonadMask m, MonadLogger m
  )
  -- | Options
  => PostgresNixOptions
  -> (FilePath -> m a)
  -> m a
withPostgresUnixSocketViaNix (PostgresNixOptions {..}) action = do
  postgresBinDir <- (</> "bin") <$> buildNixSymlinkJoin [postgresNixPostgres]
  withPostgresUnixSocket postgresBinDir postgresNixUsername postgresNixPassword postgresNixDatabase action

-- | The lowest-level raw process version.
withPostgresUnixSocket :: (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadFail m, MonadMask m, MonadLogger m
  )
  -- | Postgres binary dir
  => FilePath
  -- | Username
  -> Text
  -- | Password
  -> Text
  -- | Database
  -> Text
  -- | Action callback
  -> (FilePath -> m a)
  -> m a
withPostgresUnixSocket postgresBinDir username password database action = do
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
            liftIO $ T.hPutStrLn h password
            hClose h
            createProcessWithLogging ((proc (postgresBinDir </> "initdb") [dbDirName
                                                                            , "--username", toString username
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
          createProcessWithLogging ((proc (postgresBinDir </> "pg_ctl") [
                                        "-D", dbDirName
                                        , "-l", logfileName
                                        , "-o", [i|--unix_socket_directories='#{unixSockDir}'|]
                                        , "start" , "--wait"
                                        ]) { cwd = Just dir })
            >>= waitForProcess >>= (`shouldBe` ExitSuccess)

          -- Create the default db
          createProcessWithLogging ((proc (postgresBinDir </> "psql") [
                                        -- "-h", unixSockDir
                                        -- , "--username", toString postgresNixUsername
                                        [i|postgresql://#{username}:#{password}@/?host=#{unixSockDir}|]
                                        , "-c", [i|CREATE DATABASE #{database};|]
                                        ]) { cwd = Just dir })
            >>= waitForProcess >>= (`shouldBe` ExitSuccess)


          files <- listDirectory unixSockDir
          filterM ((isSocket <$>) . liftIO . getFileStatus) [unixSockDir </> f | f <- files] >>= \case
            [f] -> pure f
            [] -> expectationFailure [i|Couldn't find Unix socket for PostgreSQL server (check output and logfile for errors).|]
            xs -> expectationFailure [i|Found multiple Unix sockets for PostgreSQL server, not sure which one to use: #{xs}|]
      )
      (\_ -> do
          void $ readCreateProcessWithLogging ((proc (postgresBinDir </> "pg_ctl") [
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

-- | Introduce a PostgresSQL instance via a container (either Docker or Podman).
introducePostgresViaContainer :: (
  HasBaseContext context
  , MonadUnliftIO m, MonadMask m
  )
  -- | Options
  => PostgresContainerOptions
  -> SpecFree (LabelValue "postgres" PostgresContext :> context) m ()
  -> SpecFree context m ()
introducePostgresViaContainer opts = introduceWith "PostgreSQL via container" postgres $ \action -> do
  withPostgresContainer opts (void . action)

-- | Bracket-style variant of 'introducePostgresViaContainer'.
withPostgresContainer :: (
  HasCallStack, MonadUnliftIO m, MonadLoggerIO m, MonadMask m, HasBaseContextMonad context m
  )
  -- | Options
  => PostgresContainerOptions
  -> (PostgresContext -> m a)
  -> m a
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
  HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
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

#endif
