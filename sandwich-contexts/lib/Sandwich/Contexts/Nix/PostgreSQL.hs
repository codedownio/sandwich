{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.Nix.PostgreSQL (
  postgres
  , introducePostgres

  , postgresUnixSocket
  , introducePostgresUnixSocket

  , PostgresNixOptions
  , postgresNixPostgres
  , defaultPostgresNixOptions

  , PostgresContext(..)
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text.IO as T
import Relude hiding (withFile)
import Sandwich.Contexts.Nix
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import Test.Sandwich
import UnliftIO.Directory
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IO (hClose, withFile)
import UnliftIO.Process

-- * Labels

postgres :: Label "postgres" PostgresContext
postgres = Label

postgresUnixSocket :: Label "postgresUnixSocket" PostgresContext
postgresUnixSocket = Label

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

-- TODO: use the same context here as the container version
data PostgresContext = PostgresContext {
  postgresUsername :: Text
  , postgresPassword :: Text
  , postgresConnString :: Text
  , postgresDatabase :: Text
  } deriving (Show)


-- initdb -D mydb
-- echo "listen_addresses=''" >> mydb/postgresql.conf
-- pg_ctl -D mydb -l logfile -o "--unix_socket_directories='$PWD'" start --wait

-- pg_ctl -D mydb -l logfile stop --wait

introducePostgres :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  ) => PostgresNixOptions -> SpecFree (LabelValue "postgres" PostgresContext :> context) m () -> SpecFree context m ()
introducePostgres opts@(PostgresNixOptions {..}) = introduceWith "PostgreSQL via Nix" postgres $ \action -> do
  nixEnv <- buildNixEnvironment [postgresNixPostgres]

  withPostgresUnixSocket opts nixEnv $ \unixSocket -> do
    debug [i|Got unix socket: #{unixSocket}|]
    -- TODO: set up proxy
    void $ action $ PostgresContext {
      postgresUsername = postgresNixUsername
      , postgresPassword = postgresNixPostgres
      , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@/#{postgresNixDatabase}?host=#{takeDirectory unixSocket}|]
      , postgresDatabase = postgresNixDatabase
      }

introducePostgresUnixSocket :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  ) => PostgresNixOptions -> SpecFree (LabelValue "postgresUnixSocket" PostgresContext :> context) m () -> SpecFree context m ()
introducePostgresUnixSocket opts@(PostgresNixOptions {..}) = introduceWith "PostgreSQL via Nix" postgresUnixSocket $ \action -> do
  nixEnv <- buildNixEnvironment [postgresNixPostgres]
  withPostgresUnixSocket opts nixEnv $ \unixSocket -> do
    void $ action $ PostgresContext {
      postgresUsername = postgresNixUsername
      , postgresPassword = postgresNixPostgres
      , postgresConnString = [i|postgresql://#{postgresNixUsername}:#{postgresNixPassword}@/#{postgresNixDatabase}?host=#{takeDirectory unixSocket}|]
      , postgresDatabase = postgresNixDatabase
      }

withPostgresUnixSocket :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadFail m, MonadMask m, MonadLogger m
  ) => PostgresNixOptions -> FilePath -> (FilePath -> m a) -> m a
withPostgresUnixSocket (PostgresNixOptions {..}) nixEnv action = do
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
