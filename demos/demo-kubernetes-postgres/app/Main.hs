{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Time
import Database.PostgreSQL.Simple (Only(..), connectPostgreSQL, query_)
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Kubernetes.PostgresServer
import Test.Sandwich.Contexts.Nix


spec :: TopSpec
spec = describe "Introducing PostgreSQL on Kubernetes" $ do
  introduceNixContext nixpkgsReleaseDefault $
    introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $
    introduceBinaryViaNixPackage @"kubectl" "kubectl" $
    withKubernetesNamespace "postgres-demo" $
    introduceK8SPostgresServer ((defaultPostgresK8SOptions "postgres-demo") {
        postgresK8SImage = "docker.io/bitnami/postgresql@sha256:e93732718bf7fafa61a04abaa437fc601c80a791857105fd6fca18407b5725c9"
        , postgresK8SExtraEnv = [("POSTGRES_MAX_CONNECTIONS", "400")]
        }) $ do
      it "prints the server info" $ do
        server@(PostgresContext {..}) <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]

      it "does a simple query" $ do
        PostgresContext {..} <- getContext postgres
        selectTwoPlusTwo (encodeUtf8 postgresConnString) >>= (`shouldBe` 4)

      it "queries the current time" $ do
        PostgresContext {..} <- getContext postgres
        now <- selectUtcNow (encodeUtf8 postgresConnString)
        info [i|Got now: #{now}|]

      it "uses the configured max_connections" $ do
        PostgresContext {..} <- getContext postgres
        selectMaxConnections (encodeUtf8 postgresConnString) >>= (`shouldBe` "400")

selectTwoPlusTwo :: MonadIO m => ByteString -> m Int
selectTwoPlusTwo connString = liftIO $ do
  conn <- connectPostgreSQL connString
  [Only n] <- query_ conn "select 2 + 2"
  return n

selectUtcNow :: MonadIO m => ByteString -> m UTCTime
selectUtcNow connString = liftIO $ do
  conn <- connectPostgreSQL connString
  [Only n] <- query_ conn "select now()"
  return n

selectMaxConnections :: MonadIO m => ByteString -> m Text
selectMaxConnections connString = liftIO $ do
  conn <- connectPostgreSQL connString
  [Only n] <- query_ conn "show max_connections"
  return n


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
