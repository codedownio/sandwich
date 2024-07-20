
module Spec.NixContexts.PostgreSQL where

import Data.String.Interpolate
import Database.PostgreSQL.Simple (Only(..), connectPostgreSQL, query_)
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.PostgreSQL


tests :: TopSpec
tests = describe "PostgreSQL Nix" $ do
  introduceNixContext nixpkgsReleaseDefault $ do
    introducePostgresUnixSocketViaNix defaultPostgresNixOptions $ do
      it "should have a working postgres Unix socket" $ do
        ctx@(PostgresContext {..}) <- getContext postgres
        info [i|Got context: #{ctx}|]
        selectTwoPlusTwo (encodeUtf8 postgresConnString) >>= (`shouldBe` 4)



selectTwoPlusTwo :: MonadIO m => ByteString -> m Int
selectTwoPlusTwo connString = liftIO $ do
  conn <- connectPostgreSQL connString
  [Only n] <- query_ conn "select 2 + 2"
  return n
