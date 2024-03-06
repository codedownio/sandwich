{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.FakeSmtpServer (
  introduceFakeSmtpServer'
  , withFakeSMTPServer
  , fakeSmtpServer
  , FakeSmtpServer(..)
  , EmailInfo(..)

  , getEmails
  , authUsername
  , authPassword
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Retry
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.String.Interpolate
import Network.HTTP.Client
import Network.Socket (PortNumber)
import Relude
import Sandwich.Contexts.Files
import Sandwich.Contexts.Util.Aeson
import Sandwich.Contexts.Waits
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich
import UnliftIO.Directory
import UnliftIO.Exception


-- * Types

data EmailInfo = EmailInfo {
  emailInfoAttachments :: A.Value
  , emailInfoText :: Text
  , emailInfoTextAsHtml :: Text
  , emailInfoSubject :: Text
  , emailInfoDate :: Maybe Text
  , emailInfoTo :: A.Value
  , emailInfoFrom :: A.Value
  , emailInfoMessageId :: Maybe Text
  , emailInfoHtml :: Text
  } deriving (Show, Eq)
-- These Aeson options need to match the return values from fake_smtp_server
$(A.deriveJSON (A.defaultOptions { A.fieldLabelModifier = dropNAndCamelCase (length ("emailInfo" :: String)) }) ''EmailInfo)

data FakeSmtpServer = FakeSmtpServer {
  fakeSmtpServerSmtpPort :: PortNumber
  , fakeSmtpServerGetEmails :: forall m. (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m) => m [EmailInfo]
  }

fakeSmtpServer :: Label "fakeSmtpServer" FakeSmtpServer
fakeSmtpServer = Label

-- * Functions

introduceFakeSmtpServerNix :: (
  HasBaseContext context
  , MonadMask m, MonadUnliftIO m
  ) => Bool -> Bool -> SpecFree (LabelValue "fakeSmtpServer" FakeSmtpServer :> context) m () -> SpecFree context m ()
introduceFakeSmtpServerNix auth allowInsecureLogin =
  undefined

introduceFakeSmtpServer' :: (
  HasBaseContext context, HasFile context "fake-smtp-server"
  , MonadMask m, MonadUnliftIO m
  ) => Bool -> Bool -> SpecFree (LabelValue "fakeSmtpServer" FakeSmtpServer :> context) m () -> SpecFree context m ()
introduceFakeSmtpServer' auth allowInsecureLogin = introduceWith "fake SMTP server" fakeSmtpServer (withFakeSMTPServer auth allowInsecureLogin)

authUsername, authPassword :: Text
authUsername = "user"
authPassword = "pass"

withFakeSMTPServer :: (
  HasBaseContext context, MonadReader context m, HasFile context "fake-smtp-server"
  , MonadLoggerIO m, MonadThrow m, MonadUnliftIO m
  ) => Bool -> Bool -> (FakeSmtpServer -> m [Result]) -> m ()
withFakeSMTPServer auth allowInsecureLogin action = do
  folder <- getCurrentFolder >>= \case
    Nothing -> expectationFailure "withFakeSMTPServer must be run with a run root"
    Just x -> return x

  let httpPortFile = folder </> "http-port-file"
  let smtpPortFile = folder </> "smtp-port-file"

  fakeSmtpServerPath <- askFile @"fake-smtp-server"

  bracket (do
              let authFlag = if auth then ["--auth",  [i|#{authUsername}:#{authPassword}|]] else []
              let insecureLoginFlag = if allowInsecureLogin then "--allow-insecure-login" else ""
              createProcessWithLogging ((proc fakeSmtpServerPath ([insecureLoginFlag
                                                                  , "--smtp-port", "0"
                                                                  , "--smtp-port-file", smtpPortFile
                                                                  , "--http-port", "0"
                                                                  , "--http-port-file", httpPortFile
                                                                  ] <> authFlag)) {
                                           create_group = True
                                           })
          )
          (\p -> do
              void $ liftIO (interruptProcessGroupOf p >> waitForProcess p)
          )
          (\_ -> do
              httpPort <- waitForPortFile 120.0 httpPortFile
              smtpPort <- waitForPortFile 120.0 smtpPortFile

              let authPart = case auth of
                    True -> [i|#{authUsername}:#{authPassword}@|] :: Text
                    False -> ""

              waitUntil200WithTimeout' (1_000_000 * 60 * 2) [i|http://#{authPart}localhost:#{httpPort}/api/emails|]

              manager <- liftIO $ newManager defaultManagerSettings
              void $ action $ FakeSmtpServer smtpPort (getEmails manager authPart httpPort)
          )


waitForPortFile :: (MonadLoggerIO m) => Double -> FilePath -> m PortNumber
waitForPortFile timeoutSeconds path = do
  let policy = limitRetriesByCumulativeDelay (round (timeoutSeconds * 1_000_000)) $ capDelay 1_000_000 $ exponentialBackoff 1000
  liftIO $ recoverAll policy $ \(RetryStatus {}) -> do
    unlessM (doesPathExist path) $
      expectationFailure [i|Port file '#{path}' didn't exist yet.|]

    contents <- System.IO.readFile path
    case readMaybe contents of
      Nothing -> expectationFailure [i|Couldn't read port number: '#{contents}'|]
      Just n -> pure n

getEmails :: (
  MonadLoggerIO m, MonadUnliftIO m, MonadThrow m
  ) => Manager -> Text -> PortNumber -> m [EmailInfo]
getEmails manager authPart httpPort = do
  req <- liftIO $ parseRequest [i|http://#{authPart}localhost:#{httpPort}/api/emails|]
  try (liftIO $ httpLbs req manager) >>= \case
    Left (err :: HttpException) -> expectationFailure [i|Failed to fetch emails: #{err}|]
    Right response ->
      case A.eitherDecode (responseBody response) of
        Left err -> expectationFailure [i|Couldn't decode emails: '#{err}'. Response body  '#{responseBody response}'. Response: '#{response}'.|]
        Right (emails :: [EmailInfo]) -> return emails
