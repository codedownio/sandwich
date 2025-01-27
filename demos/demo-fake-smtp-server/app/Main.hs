{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Data.Text
import Network.HTTP.Client
import Network.HaskellNet.SMTP (AuthType(..), authenticate, doSMTPPort, sendMail)
import Network.Mail.Mime
import Network.Socket (PortNumber)
import Test.Sandwich
import Test.Sandwich.Contexts.FakeSmtpServer
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Waits
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a fake SMTP server" $
  introduceNixContext nixpkgsReleaseDefault $ introduceFakeSmtpServerNix defaultFakeSmtpServerOptions $ do
    it "sends an email and verifies it was received" $ do
      FakeSmtpServer {..} <- getContext fakeSmtpServer
      info [i|Got fake SMTP server on port: #{fakeSmtpServerSmtpPort}|]

      sendSampleEmail fakeSmtpServerHostname fakeSmtpServerSmtpPort
      waitUntil 60 $ do
        fakeSmtpServerGetEmails >>= \case
          [x] -> debug [i|Got email: #{x}|]
          xs -> expectationFailure [i|Unexpected emails result: #{xs}|]

sendSampleEmail :: (MonadLoggerIO m) => String -> PortNumber -> m ()
sendSampleEmail smtpHostname smtpPort = do
  manager <- liftIO $ newManager defaultManagerSettings

  mail <- liftIO $ simpleMailWithImages [Address (Just "To User") "to@codedown.io"] "from@codedown.io" "Subject" "Text body" "HTML body" [] []

  let shouldAuth = False
  -- let disableCertValidation = False

  let smtpUsername = "username"
  let smtpPassword = "password"

  let doSMTPFn = doSMTPPort smtpHostname (fromIntegral smtpPort)
  -- let doSMTPFn = doSMTPSSLWithSettings smtpHostname (defaultSettingsSMTPSSL {sslPort=(fromIntegral smtpPort), sslDisableCertificateValidation=disableCertValidation})
  -- let doSMTPFn = doSMTPSTARTTLSWithSettings smtpHostname (defaultSettingsSMTPSTARTTLS {sslPort=(fromIntegral smtpPort), sslDisableCertificateValidation=disableCertValidation})

  liftIO $ doSMTPFn $ \smtpConn -> do
    authSucceed <- if shouldAuth then authenticate PLAIN smtpUsername smtpPassword smtpConn else return True
    case authSucceed of
      True -> do
        forM_ (mailTo mail) $ \(Address _ to) -> do
          let Address _ from = mailFrom mail
          sendMail mail smtpConn
      False -> expectationFailure [i|Failed to authenticate to SMTP server #{smtpHostname} (port #{smtpPort}) with username #{smtpUsername}|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
