
module Spec.NixContexts.PostgreSQL where

import Sandwich.Contexts.Nix
import Sandwich.Contexts.Nix.PostgreSQL
import Data.String.Interpolate
import Database.PostgreSQL.Simple
import Relude
import Test.Sandwich


tests :: TopSpec
tests = describe "PostgreSQL Nix" $ do
  introduceNixContext nixpkgsReleaseDefault $ do
    introducePostgresUnixSocket defaultPostgresNixOptions $ do
      it "should have a working postgres Unix socket" $ do
        ctx@(PostgresContext {..}) <- getContext postgresUnixSocket
        info [i|Got context: #{ctx}|]
        selectTwoPlusTwo (encodeUtf8 postgresConnString) >>= (`shouldBe` 4)



selectTwoPlusTwo :: MonadIO m => ByteString -> m Int
selectTwoPlusTwo connString = liftIO $ do
  conn <- connectPostgreSQL connString
  [Only n] <- query_ conn "select 2 + 2"
  return n
