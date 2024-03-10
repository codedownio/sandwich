{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
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
      it "prints the MinIO server info" $ do
        server <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]

  describe "Via Nix" $
    introduceNixContext nixpkgsReleaseDefault $ introducePostgresViaNix defaultPostgresNixOptions $ do
      it "prints the MinIO server info" $ do
        server <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]

  describe "Via container" $
    introducePostgresViaContainer defaultPostgresContainerOptions $ do
      it "prints the MinIO server info" $ do
        server <- getContext postgres
        info [i|Got PostgreSQL server: #{server}|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
