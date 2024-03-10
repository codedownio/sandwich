{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Database.PostgreSQL.Simple
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.FakeSmtpServer
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.PostgreSQL
import Test.Sandwich.Contexts.Waits


spec :: TopSpec
spec = describe "Introducing PostgreSQL" $ do
  describe "Unix socket Via Nix" $
    introduceNixContext nixpkgsReleaseDefault $ introducePostgresUnixSocketViaNix defaultPostgresNixOptions $ do
      it "prints the server info and does a simple test" $ do
        server@(PostgresContext {..}) <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]

        selectTwoPlusTwo (encodeUtf8 postgresConnString) >>= (`shouldBe` 4)

  describe "Via Nix" $
    introduceNixContext nixpkgsReleaseDefault $ introducePostgresViaNix defaultPostgresNixOptions $ do
      it "prints the server info and does a simple test" $ do
        server@(PostgresContext {..}) <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]

        selectTwoPlusTwo (encodeUtf8 postgresConnString) >>= (`shouldBe` 4)

  describe "Via container" $
    introducePostgresViaContainer defaultPostgresContainerOptions $ do
      it "prints the server info and does a simple test" $ do
        server@(PostgresContext {..}) <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]

        -- TODO: this sometimes fails, is the waitForHealth call not sufficient?

        selectTwoPlusTwo (encodeUtf8 postgresConnString) >>= (`shouldBe` 4)


selectTwoPlusTwo :: MonadIO m => ByteString -> m Int
selectTwoPlusTwo connString = liftIO $ do
  conn <- connectPostgreSQL connString
  [Only n] <- query_ conn "select 2 + 2"
  return n


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
