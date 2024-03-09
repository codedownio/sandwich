{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Test.Sandwich.Contexts.FakeSmtpServer
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.MinIO
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
import Test.Sandwich


spec :: TopSpec
spec = describe "Introducing a fake SMTP server" $
  introduceNixContext nixpkgsReleaseDefault $ introduceMinIONix defaultMinIOContextOptions $ do
    it "prints the MinIO server info" $ do
      server <- getContext fakeS3Server
      info [i|Got S3 server: #{server}|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
