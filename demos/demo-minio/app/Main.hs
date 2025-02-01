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
import Test.Sandwich.Contexts.Container
import Test.Sandwich.Contexts.FakeSmtpServer
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.MinIO
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Waits


spec :: TopSpec
spec = describe "Introducing MinIO" $ do
  describe "Via Nix" $
    introduceNixContext nixpkgsReleaseDefault $ introduceMinIOViaNix defaultMinIOContextOptions $ do
      it "prints the MinIO server info" $ do
        server <- getContext testS3Server
        info [i|Got S3 server: #{server}|]

  describe "Via container" $
    introduceMinIOViaContainer defaultMinIOContextOptions defaultContainerOptions $ do
      it "prints the MinIO server info" $ do
        server <- getContext testS3Server
        info [i|Got S3 server: #{server}|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
