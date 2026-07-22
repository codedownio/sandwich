{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Provides a MySQL/MariaDB database, using a suitable package from Nix. The server
is started listening on a Unix socket (short path) and exposed to the test as a
TCP address via a reverse proxy, matching "Test.Sandwich.Contexts.PostgreSQL".

Both MariaDB and Oracle MySQL are supported: the flavor is detected from the Nix
package's binaries (MariaDB ships @mariadb-install-db@; MySQL does not), and the
appropriate init/start commands are used for each.
-}
module Test.Sandwich.Contexts.MySQL (
  -- * Nix
  introduceMysqlViaNix
  , withMysqlViaNix
  , withMysqlViaNix'

  -- * Options
  , MysqlNixOptions(..)
  , defaultMysqlNixOptions
  , MysqlAuthPlugin(..)

  -- * Types
  , mysql
  , MysqlContext(..)
  , MysqlVariant(..)

  , NetworkAddress(..)
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import Relude
import System.Exit
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.ReverseProxy.TCP
import Test.Sandwich.Contexts.Types.Network
import Test.Sandwich.Contexts.UnixSocketPath
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process


-- * Labels

mysql :: Label "mysql" MysqlContext
mysql = Label

-- * Types

data MysqlNixOptions = MysqlNixOptions {
  -- | MariaDB or Oracle MySQL package to use within the Nixpkgs snapshot of your
  -- 'NixContext'. Defaults to @mariadb@; @mysql80@ (etc.) selects Oracle MySQL.
  mysqlNixPackage :: Text
  -- | Username to create. Default @test@.
  , mysqlNixUsername :: Text
  -- | Password for the created user. Default @test@.
  , mysqlNixPassword :: Text
  -- | Database to create. Default @test@.
  , mysqlNixDatabase :: Text
  -- | Auth plugin for the created user, on the Oracle MySQL flavor only (MariaDB
  -- always uses native password). Default 'NativePassword'.
  , mysqlNixAuthPlugin :: MysqlAuthPlugin
  }
defaultMysqlNixOptions :: MysqlNixOptions
defaultMysqlNixOptions = MysqlNixOptions {
  mysqlNixPackage = "mariadb"
  , mysqlNixUsername = "test"
  , mysqlNixPassword = "test"
  , mysqlNixDatabase = "test"
  , mysqlNixAuthPlugin = NativePassword
  }

-- | MySQL authentication plugin for the created user. MySQL 8 defaults new users
-- to @caching_sha2_password@; 'NativePassword' forces the simpler
-- @mysql_native_password@ (what most lightweight clients and credential-forwarding
-- proxies speak).
data MysqlAuthPlugin = NativePassword | CachingSha2Password
  deriving (Show, Eq)

-- | Which flavor of server a Nix package provides.
data MysqlVariant = MariaDB | MySQL
  deriving (Show, Eq)

data MysqlContext = MysqlContext {
  mysqlUsername :: Text
  , mysqlPassword :: Text
  , mysqlDatabase :: Text
  , mysqlVariant :: MysqlVariant
  , mysqlAddress :: NetworkAddress
  , mysqlConnString :: Text
  } deriving (Show)


-- | Introduce a MySQL/MariaDB instance, using a suitable package from Nix.
introduceMysqlViaNix :: (
  HasBaseContext context, HasNixContext context
  , MonadUnliftIO m, MonadMask m
  )
  -- | Options
  => MysqlNixOptions
  -> SpecFree (LabelValue "mysql" MysqlContext :> context) m ()
  -> SpecFree context m ()
introduceMysqlViaNix opts = introduceWith "MySQL via Nix" mysql $ \action ->
  withMysqlViaNix opts (void . action)

-- | Bracket-style variant of 'introduceMysqlViaNix'.
withMysqlViaNix :: (
  HasBaseContextMonad context m, HasNixContext context
  , MonadUnliftIO m, MonadMask m, MonadFail m, MonadLoggerIO m
  )
  -- | Options
  => MysqlNixOptions
  -> (MysqlContext -> m a)
  -> m a
withMysqlViaNix opts action = do
  nc <- getContext nixContext
  withMysqlViaNix' nc opts action

-- | Lower-level variant of 'withMysqlViaNix'.
withMysqlViaNix' :: (
  HasBaseContextMonad context m
  , MonadUnliftIO m, MonadMask m, MonadFail m, MonadLoggerIO m
  )
  -- | Nix context
  => NixContext
  -- | Options
  -> MysqlNixOptions
  -> (MysqlContext -> m a)
  -> m a
withMysqlViaNix' nc (MysqlNixOptions {..}) action = do
  binDir <- (</> "bin") <$> buildNixPackage' nc mysqlNixPackage
  variant <- detectVariant binDir
  Just dir <- getCurrentFolder
  baseDir <- liftIO $ createTempDirectory dir "mysql-nix"
  let dataDir = baseDir </> "data"

  -- The Unix socket path has a short length limit (~107 bytes), so keep it out of
  -- the (potentially long) sandwich test tree.
  withUnixSocketDirectory "mysql-sock" 20 $ \sockDir -> do
    let sock = sockDir </> "mysqld.sock"
    bracket
      (do
          -- Initialize the data directory (root authenticates over the socket).
          case variant of
            MariaDB ->
              runProc dir binDir "mariadb-install-db" [
                "--no-defaults"
                , "--datadir=" <> dataDir
                , "--auth-root-authentication-method=normal"
                ]
            MySQL -> do
              createDirectoryIfMissing True dataDir
              runProc dir binDir "mysqld" [
                "--no-defaults"
                , "--initialize-insecure"
                , "--datadir=" <> dataDir
                ]

          -- Start the server on the Unix socket only (TCP comes from the proxy).
          (_, serverAsy) <- createProcessWithLogging ((proc (binDir </> serverBin variant) (
            [ "--no-defaults"
            , "--datadir=" <> dataDir
            , "--socket=" <> sock
            , "--skip-networking"
            , "--pid-file=" <> (baseDir </> "mysqld.pid")
            ] <> serverExtraArgs variant)) { cwd = Just dir })

          waitForReady binDir sock

          -- Create the user + database.
          runProc dir binDir "mysql" [
            "--no-defaults", "--socket=" <> sock
            , "-u", "root"
            , "-e", toString (setupSql variant)
            ]

          pure serverAsy
      )
      (\serverAsy -> do
          void $ tryAny $ readProcessWithExitCode (binDir </> "mysqladmin") [
            "--no-defaults", "--socket=" <> sock
            , "-u", "root"
            , "shutdown"
            ] ""
          cancel serverAsy)
      (\_ ->
          withProxyToUnixSocket sock $ \port ->
            action MysqlContext {
              mysqlUsername = mysqlNixUsername
              , mysqlPassword = mysqlNixPassword
              , mysqlDatabase = mysqlNixDatabase
              , mysqlVariant = variant
              , mysqlAddress = NetworkAddressTCP "localhost" (fromIntegral port)
              , mysqlConnString = [i|mysql://#{mysqlNixUsername}:#{mysqlNixPassword}@localhost:#{port}/#{mysqlNixDatabase}|]
              })
  where
    -- MariaDB always uses native password auth. On MySQL, the auth plugin is chosen
    -- by 'mysqlNixAuthPlugin' (default native, so lightweight clients and
    -- credential-forwarding proxies can authenticate; caching_sha2 is opt-in).
    setupSql variant = T.concat
      [ "CREATE DATABASE IF NOT EXISTS ", mysqlNixDatabase, "; "
      , "CREATE USER '", mysqlNixUsername, "'@'%' ", authClause variant, "; "
      , "CREATE USER '", mysqlNixUsername, "'@'localhost' ", authClause variant, "; "
      , "GRANT ALL PRIVILEGES ON ", mysqlNixDatabase, ".* TO '", mysqlNixUsername, "'@'%'; "
      , "GRANT ALL PRIVILEGES ON ", mysqlNixDatabase, ".* TO '", mysqlNixUsername, "'@'localhost'; "
      , "FLUSH PRIVILEGES;"
      ]
    authClause MariaDB = "IDENTIFIED BY '" <> mysqlNixPassword <> "'"
    authClause MySQL = "IDENTIFIED WITH " <> pluginName mysqlNixAuthPlugin <> " BY '" <> mysqlNixPassword <> "'"
    pluginName NativePassword = "mysql_native_password"
    pluginName CachingSha2Password = "caching_sha2_password"

-- | Detect the server flavor from the package's binaries: only MariaDB ships
-- @mariadb-install-db@.
detectVariant :: MonadIO m => FilePath -> m MysqlVariant
detectVariant binDir = do
  isMaria <- doesFileExist (binDir </> "mariadb-install-db")
  pure $ if isMaria then MariaDB else MySQL

serverBin :: MysqlVariant -> FilePath
serverBin MariaDB = "mariadbd"
serverBin MySQL = "mysqld"

-- | Disable MySQL's X plugin so it doesn't try to open its own network port.
serverExtraArgs :: MysqlVariant -> [String]
serverExtraArgs MariaDB = []
serverExtraArgs MySQL = ["--mysqlx=0"]

-- | Poll @mysqladmin ping@ until the server accepts connections.
waitForReady :: (MonadUnliftIO m, MonadLoggerIO m) => FilePath -> FilePath -> m ()
waitForReady binDir sock = go (150 :: Int)
  where
    go 0 = expectationFailure "MySQL server did not become ready in time"
    go n = do
      (code, _, _) <- liftIO $ readProcessWithExitCode (binDir </> "mysqladmin") [
        "--no-defaults", "--socket=" <> sock, "-u", "root", "ping"
        ] ""
      case code of
        ExitSuccess -> pure ()
        _ -> threadDelay 200_000 >> go (n - 1)

runProc :: (HasBaseContextMonad context m, MonadUnliftIO m, MonadLoggerIO m) => FilePath -> FilePath -> FilePath -> [String] -> m ()
runProc dir binDir binName args = do
  (ps, asy) <- createProcessWithLogging ((proc (binDir </> binName) args) { cwd = Just dir })
  finally (waitForProcess ps >>= (`shouldBe` ExitSuccess)) (cancel asy)
