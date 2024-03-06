{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.String.Interpolate
import Sandwich.Contexts.FakeSmtpServer
import Sandwich.Contexts.Files
import Sandwich.Contexts.Nix
import Test.Sandwich
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a fake SMTP server" $
  introduceNixContext nixpkgsReleaseDefault $ introduceFakeSmtpServerNix defaultFakeSmtpServerOptions $ do
    it "prints the SMTP server port" $ do
      server <- getContext fakeSmtpServer
      info [i|Got fake SMTP server on port: #{fakeSmtpServerSmtpPort server}|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
