{-# LANGUAGE TypeOperators #-}

module Main where

import Data.String.Interpolate
import Database.MySQL.Base
import Relude
import qualified System.IO.Streams as Streams
import Test.Sandwich
import Test.Sandwich.Contexts.MySQL
import Test.Sandwich.Contexts.Nix


spec :: TopSpec
spec = describe "Introducing MySQL" $
  introduceNixContext nixpkgsReleaseDefault $ do
    describe "MariaDB" $
      introduceMysqlViaNix defaultMysqlNixOptions { mysqlNixPackage = "mariadb" } simpleQuerySpec

    describe "MySQL" $
      introduceMysqlViaNix defaultMysqlNixOptions { mysqlNixPackage = "mysql80" } simpleQuerySpec


simpleQuerySpec :: MonadIO m => SpecFree (LabelValue "mysql" MysqlContext :> context) m ()
simpleQuerySpec = it "prints the server info and does a simple query" $ do
  server@(MysqlContext {..}) <- getContext mysql
  info [i|Got #{mysqlVariant} server: #{server}|]

  result <- liftIO $ selectTwoPlusTwo server
  info [i|Got 2 + 2 = #{result}|]
  result `shouldBe` 4


selectTwoPlusTwo :: MysqlContext -> IO Int
selectTwoPlusTwo (MysqlContext {..}) = do
  let port = case mysqlAddress of
        NetworkAddressTCP _ p -> fromIntegral p
        NetworkAddressUnix _ -> 3306
  conn <- connect defaultConnectInfo {
    ciHost = "127.0.0.1"
    , ciPort = port
    , ciUser = encodeUtf8 mysqlUsername
    , ciPassword = encodeUtf8 mysqlPassword
    , ciDatabase = encodeUtf8 mysqlDatabase
    }
  (_, valuesStream) <- query_ conn "SELECT 2 + 2"
  rows <- Streams.toList valuesStream
  close conn
  pure $ case rows of
    [[v]] -> mysqlValueToInt v
    _ -> -1

mysqlValueToInt :: MySQLValue -> Int
mysqlValueToInt = \case
  MySQLInt8U n -> fromIntegral n
  MySQLInt8 n -> fromIntegral n
  MySQLInt16U n -> fromIntegral n
  MySQLInt16 n -> fromIntegral n
  MySQLInt32U n -> fromIntegral n
  MySQLInt32 n -> fromIntegral n
  MySQLInt64U n -> fromIntegral n
  MySQLInt64 n -> fromIntegral n
  _ -> -1


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
