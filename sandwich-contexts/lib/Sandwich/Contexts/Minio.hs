{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Sandwich.Contexts.Minio (
  MinioContextOptions (..)
  , defaultMinioContextOptions

  , introduceMinioNix
  , withMinioViaBinary

  , introduceMinioContainer
  , withMinioContainer

  , fakeS3Server
  , FakeS3Server(..)
  , HasFakeS3Server
  , HttpMode(..)

  , fakeS3ServerEndpoint
  , fakeS3TestEndpoint
  , fakeS3ConnectionInfo
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Retry
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.TypeLits
import Network.HostName
import Network.Minio
import Network.Socket (PortNumber)
import Network.URI (parseURI)
import Relude
import Safe
import Sandwich.Contexts.Files
import Sandwich.Contexts.Nix
import Sandwich.Contexts.Util.Aeson
import Sandwich.Contexts.Util.Container
import Sandwich.Contexts.Util.UUID
import Sandwich.Contexts.Waits
import System.Exit
import System.FilePath
import System.IO.Temp
import Test.Sandwich
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process


-- * Types

fakeS3Server :: Label "fakeS3Server" FakeS3Server
fakeS3Server = Label

data FakeS3Server = FakeS3Server {
  fakeS3ServerHostname :: HostName
  , fakeS3ServerPort :: PortNumber
  , fakeS3ServerAccessKeyId :: Text
  , fakeS3ServerSecretAccessKey :: Text
  , fakeS3Bucket :: Text
  , fakeS3TestHostname :: HostName
  , fakeS3TestPort :: PortNumber
  , fakeS3HttpMode :: HttpMode
  } deriving (Show, Eq)

data HttpMode = HttpModeHttp | HttpModeHttps | HttpModeHttpsNoValidate
  deriving (Show, Eq)

type HasFakeS3Server context = HasLabel context "fakeS3Server" FakeS3Server

fakeS3ServerEndpoint :: FakeS3Server -> Text
fakeS3ServerEndpoint (FakeS3Server {..}) = [i|#{protocol}://#{fakeS3ServerHostname}:#{fakeS3ServerPort}|]
  where protocol :: Text = if fakeS3HttpMode == HttpModeHttp then "http" else "https"

fakeS3TestEndpoint :: FakeS3Server -> Text
fakeS3TestEndpoint (FakeS3Server {..}) = [i|#{protocol}://#{fakeS3TestHostname}:#{fakeS3TestPort}|]
  where protocol :: Text = if fakeS3HttpMode == HttpModeHttp then "http" else "https"

fakeS3ConnectionInfo :: FakeS3Server -> ConnectInfo
fakeS3ConnectionInfo fakeServ@(FakeS3Server {..}) =
  fromString (toString (fakeS3TestEndpoint fakeServ))
  & setCreds (CredentialValue (AccessKey fakeS3ServerAccessKeyId) (SecretKey (fromString (toString fakeS3ServerSecretAccessKey))) Nothing)
  & (if fakeS3HttpMode == HttpModeHttpsNoValidate then disableTLSCertValidation else id)

data MinioContextOptions = MinioContextOptions {
  minioContextLabels :: Map Text Text
  , minioContextContainerName :: Maybe Text
  , minioContextContainerSystem :: ContainerSystem
  } deriving (Show, Eq)
defaultMinioContextOptions :: MinioContextOptions
defaultMinioContextOptions = MinioContextOptions {
  minioContextLabels = mempty
  , minioContextContainerName = Nothing
  , minioContextContainerSystem = ContainerSystemPodman
  }

-- * Raw

introduceMinioNix :: (
  HasBaseContext context, HasNixContext context, MonadMask m, MonadUnliftIO m
  ) => MinioContextOptions
    -> SpecFree (LabelValue "fakeS3Server" FakeS3Server :> LabelValue (AppendSymbol "file-" "minio") (EnvironmentFile "minio") :> context) m ()
    -> SpecFree context m ()
introduceMinioNix options = introduceBinaryViaNixPackage @"minio" "minio" .
  introduceWith "Minio S3 server" fakeS3Server (withMinioViaBinary options)

withMinioViaBinary :: (
  MonadReader context m, HasBaseContext context, HasFile context "minio"
  , MonadLoggerIO m, MonadMask m, MonadUnliftIO m
  ) => MinioContextOptions -> (FakeS3Server -> m [Result]) -> m ()
withMinioViaBinary (MinioContextOptions {..}) action = do
  dir <- getCurrentFolder >>= \case
    Nothing -> expectationFailure "withMinioViaBinary must be run with a current directory."
    Just x -> return x

  minioDir <- liftIO $ createTempDirectory dir "minio-storage"

  minioPath <- askFile @"minio"

  (hReadErr, hWriteErr) <- liftIO createPipe
  let cp = proc minioPath [
        "server"
        , minioDir
        , "--address", ":0"
        , "--json"
        ]

  let forwardStderr = forever $ do
        line <- liftIO (T.hGetLine hReadErr)
        debug [i|minio stderr: #{line}|]

  let bucket = "bucket1"

  withAsync forwardStderr $ \_ ->
    withCreateProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle hWriteErr }) $ \_ (Just sout) _ p -> do
      uriToUse <- fix $ \loop -> do
        line <- liftIO $ T.hGetLine sout
        debug [i|minio: #{line}|]
        case A.eitherDecode (encodeUtf8 line) of
          Right (A.Object ( aesonLookup "message" -> Just (A.String t) ))
            | "S3-API:" `T.isPrefixOf` t -> do
                let uris = t
                         & T.drop (T.length "S3-API:")
                         & T.words
                         & mapMaybe (parseURI . toString)
                undefined
            | otherwise -> loop
          _ -> loop

      let localPort = undefined

      let server = FakeS3Server {
            fakeS3ServerHostname = "127.0.0.1"
            , fakeS3ServerPort = localPort -- TODO: this needs to be innerPort if ever accessed from another container
            , fakeS3ServerAccessKeyId = "minioadmin"
            , fakeS3ServerSecretAccessKey = "minioadmin"
            , fakeS3Bucket = bucket
            , fakeS3TestHostname = "127.0.0.1"
            , fakeS3TestPort = localPort
            , fakeS3HttpMode = HttpModeHttp
            }

      waitForMinioReady server

      void $ action server

-- * Container

introduceMinioContainer :: (
  HasBaseContext context, MonadMask m, MonadUnliftIO m
  ) => MinioContextOptions -> SpecFree (LabelValue "fakeS3Server" FakeS3Server :> context) m () -> SpecFree context m ()
introduceMinioContainer options = introduceWith "Minio S3 server" fakeS3Server $ \action -> do
  withMinioContainer options action

withMinioContainer :: (
  MonadReader context m, HasBaseContext context
  , MonadLoggerIO m, MonadMask m, MonadUnliftIO m
  ) => MinioContextOptions -> (FakeS3Server -> m [Result]) -> m ()
withMinioContainer (MinioContextOptions {..}) action = do
  folder <- getCurrentFolder >>= \case
    Nothing -> expectationFailure "withMinioContainer must be run with a current directory."
    Just x -> return x

  let mockDir = folder </> "mock_root"
  createDirectoryIfMissing True mockDir
  liftIO $ void $ readCreateProcess (proc "chmod" ["777", mockDir]) "" -- Fix permission problems on GitHub Runners

  let bucket = "bucket1"

  let innerPort = 9000 :: PortNumber

  uuid <- makeUUID
  let containerName = fromMaybe ("test-s3-" <> uuid) minioContextContainerName

  let labelArgs = case minioContextLabels of
        x | M.null x -> []
        xs -> "--label" : [[i|#{k}=#{v}|] | (k, v) <- M.toList xs]

  bracket (do
              uid <- liftIO getCurrentUID

              let cp = proc (show minioContextContainerSystem) $ [
                    "run"
                    , "-d"
                    , "-p", [i|#{innerPort}|]
                    , "-v", [i|#{mockDir}:/data|]
                    , "-u", [i|#{uid}|]
                    , "--name", toString containerName
                    ]
                    <> labelArgs
                    <> [
                        "minio/minio:RELEASE.2022-09-25T15-44-53Z"
                        , "server", "/data", "--console-address", ":9001"
                    ]

              info [i|Got command: #{cp}"|]

              createProcessWithLogging cp
          )
          (\_ -> do
              void $ liftIO $ readCreateProcess (shell [i|#{minioContextContainerSystem} rm -f --volumes #{containerName}|]) ""
          )
          (\p -> do
              waitForProcess p >>= \case
                ExitSuccess -> return ()
                ExitFailure n -> expectationFailure [i|Failed to start Minio container (exit code #{n})|]

              localPort <- containerPortToHostPort minioContextContainerSystem containerName innerPort

              let server = FakeS3Server {
                    fakeS3ServerHostname = "127.0.0.1"
                    , fakeS3ServerPort = localPort -- TODO: this needs to be innerPort if ever accessed from another container
                    , fakeS3ServerAccessKeyId = "minioadmin"
                    , fakeS3ServerSecretAccessKey = "minioadmin"
                    , fakeS3Bucket = bucket
                    , fakeS3TestHostname = "127.0.0.1"
                    , fakeS3TestPort = localPort
                    , fakeS3HttpMode = HttpModeHttp
                    }

              waitForMinioReady server

              void $ action server
          )


waitForMinioReady :: (MonadLogger m, MonadUnliftIO m, MonadMask m) => FakeS3Server -> m ()
waitForMinioReady server@(FakeS3Server {..}) = do
  -- The minio image seems not to have a healthcheck?
  -- waitForHealth containerName
  waitUntilStatusCodeWithTimeout' (1_000_000 * 60 * 5) (2, 0, 0) NoVerify [i|http://#{fakeS3TestHostname}:#{fakeS3TestPort}/minio/health/live|]

  let connInfo :: ConnectInfo = setCreds (CredentialValue "minioadmin" "minioadmin" Nothing) [i|http://#{fakeS3TestHostname}:#{fakeS3TestPort}|]
  -- Make the test bucket, retrying on ServiceErr
  let policy = limitRetriesByCumulativeDelay (1_000_000 * 60 * 5) $ capDelay 1_000_000 $ exponentialBackoff 50_000
  let handlers = [\_ -> MC.Handler (\case (ServiceErr {}) -> return True; _ -> return False)
                 , \_ -> MC.Handler (\case (MErrService (ServiceErr {})) -> return True; _ -> return False)]
  debug [i|Starting to try to make bucket at http://#{fakeS3TestHostname}:#{fakeS3TestPort}|]
  recovering policy handlers $ \retryStatus@(RetryStatus {}) -> do
    info [i|About to try making S3 bucket with retry status: #{retryStatus}|]
    liftIO $ doMakeBucket connInfo fakeS3Bucket
  debug [i|Got Minio S3 server: #{server}|]


doMakeBucket :: ConnectInfo -> Bucket -> IO ()
doMakeBucket connInfo bucket = do
  result <- runMinio connInfo $ do
    try (makeBucket bucket Nothing) >>= \case
      Left BucketAlreadyOwnedByYou -> return ()
      Left e -> throwIO e
      Right _ -> return ()

  whenLeft_ result throwIO

getCurrentUID :: (HasCallStack, MonadIO m) => m Int
getCurrentUID = (readMay <$> (readCreateProcess (proc "id" ["-u"]) "")) >>= \case
  Nothing -> expectationFailure [i|Couldn't parse UID|]
  Just x -> return x
