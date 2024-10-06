{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

This module provides functions for introducing a mock SMTP server, represented by 'FakeSmtpServer'.

If you send emails to this server, you can read them out to confirm they were received correctly.

-}

module Test.Sandwich.Contexts.FakeSmtpServer (
  -- * Introduce a fake SMTP server
  introduceFakeSmtpServerNix
  , introduceFakeSmtpServerNix'
  , introduceFakeSmtpServer

  -- * Bracket-style version
  , withFakeSMTPServer

  -- * Nix derivation
  , fakeSmtpServerDerivation

  -- * Types
  , fakeSmtpServer
  , FakeSmtpServerOptions(..)
  , defaultFakeSmtpServerOptions
  , FakeSmtpServer(..)
  , EmailInfo(..)
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
import GHC.TypeLits
import Network.HTTP.Client
import Network.Socket (PortNumber)
import Relude
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich
import Test.Sandwich.Contexts.FakeSmtpServer.Derivation
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Util.Aeson
import Test.Sandwich.Contexts.Waits
import UnliftIO.Directory
import UnliftIO.Exception


-- * Types

data FakeSmtpServerOptions = FakeSmtpServerOptions {
  -- | Username and password. If not provided, the server will not be configured with authentication.
  fakeSmtpServerAuth :: Maybe (String, String)
  -- | Whether to allow insecure login.
  , fakeSmtpServerAllowInsecureLogin :: Bool
  } deriving (Show, Eq)

defaultFakeSmtpServerOptions :: FakeSmtpServerOptions
defaultFakeSmtpServerOptions = FakeSmtpServerOptions {
  fakeSmtpServerAuth = Just ("user", "password")
  , fakeSmtpServerAllowInsecureLogin = True
  }

-- | An email, as received by the server.
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
  -- | The port on which the fake SMTP server is running.
  fakeSmtpServerSmtpPort :: PortNumber
  -- | Callback to retrieve the emails the server has received.
  , fakeSmtpServerGetEmails :: forall m. (MonadLoggerIO m, MonadUnliftIO m, MonadThrow m) => m [EmailInfo]
  }

fakeSmtpServer :: Label "fakeSmtpServer" FakeSmtpServer
fakeSmtpServer = Label

-- * Functions

type BaseMonad context m = (HasBaseContext context, MonadMask m, MonadUnliftIO m)

type FakeSmtpServerContext context =
  LabelValue "fakeSmtpServer" FakeSmtpServer
  :> LabelValue (AppendSymbol "file-" "fake-smtp-server") (EnvironmentFile "fake-smtp-server")
  :> context

-- | Introduce a fake SMTP server using a Nix derivation hardcoded into this package as 'fakeSmtpServerDerivation'.
introduceFakeSmtpServerNix :: (
  BaseMonad context m, HasNixContext context
  )
  -- | Options
  => FakeSmtpServerOptions
  -- | Child spec
  -> SpecFree (FakeSmtpServerContext context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceFakeSmtpServerNix = introduceFakeSmtpServerNix' fakeSmtpServerDerivation

-- | Same as 'introduceFakeSmtpServerNix', but allows you to specify the derivation.
introduceFakeSmtpServerNix' :: (
  BaseMonad context m, HasNixContext context
  )
  -- | Nix derivation
  => Text
  -- | Options
  -> FakeSmtpServerOptions
  -- | Child spec
  -> SpecFree (FakeSmtpServerContext context) m ()
  -- | Parent spec
  -> SpecFree context m ()
introduceFakeSmtpServerNix' derivation options =
  introduceBinaryViaNixDerivation @"fake-smtp-server" derivation . introduceFakeSmtpServer options

-- | Introduce a fake SMTP server given a binary already available via 'HasFile'.
introduceFakeSmtpServer :: (
  BaseMonad context m, HasFile context "fake-smtp-server"
  )
  -- | Options
  => FakeSmtpServerOptions
  -> SpecFree (LabelValue "fakeSmtpServer" FakeSmtpServer :> context) m ()
  -> SpecFree context m ()
introduceFakeSmtpServer options = introduceWith "fake SMTP server" fakeSmtpServer (withFakeSMTPServer options)

-- | Bracket-style version of 'introduceFakeSmtpServer'.
withFakeSMTPServer :: (
  BaseMonad context m, MonadReader context m, MonadLoggerIO m, HasFile context "fake-smtp-server"
  )
  -- | Options
  => FakeSmtpServerOptions
  -> (FakeSmtpServer -> m [Result])
  -> m ()
withFakeSMTPServer (FakeSmtpServerOptions {..}) action = do
  folder <- getCurrentFolder >>= \case
    Nothing -> expectationFailure "withFakeSMTPServer must be run with a run root"
    Just x -> return x

  let httpPortFile = folder </> "http-port-file"
  let smtpPortFile = folder </> "smtp-port-file"

  fakeSmtpServerPath <- askFile @"fake-smtp-server"

  bracket (do
              let authFlag = case fakeSmtpServerAuth of
                    Just (username, password) -> ["--auth",  [i|#{username}:#{password}|]]
                    Nothing -> []
              let insecureLoginFlag = if fakeSmtpServerAllowInsecureLogin then "--allow-insecure-login" else ""
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

              let authPart = case fakeSmtpServerAuth of
                    Just (username, password) -> [i|#{username}:#{password}@|] :: Text
                    Nothing -> ""

              waitUntilStatusCodeWithTimeout (2, 0, 0) (1_000_000 * 60 * 2) YesVerify [i|http://#{authPart}localhost:#{httpPort}/api/emails|]

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
