{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Sandwich.Contexts.MinIO (
  MinIOContextOptions (..)
  , defaultMinIOContextOptions

  , introduceMinIONix
  , withMinIOViaBinary
  , withMinIO

  , introduceMinIOContainer
  , withMinIOContainer

  , fakeS3Server
  , FakeS3Server(..)
  , HasFakeS3Server
  , HttpMode(..)

  , fakeS3ServerEndpoint
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
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.TypeLits
import Network.HostName
import Network.Minio
import Network.Socket (PortNumber)
import Network.URI (URI(..), URIAuth(..), parseURI)
import Relude
import Safe
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Util.Aeson
import Test.Sandwich.Contexts.Util.Container
import Test.Sandwich.Contexts.Util.UUID
import Test.Sandwich.Contexts.Waits
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
  , fakeS3ServerBucket :: Maybe Text
  , fakeS3ServerHttpMode :: HttpMode
  } deriving (Show, Eq)

data HttpMode = HttpModeHttp | HttpModeHttps | HttpModeHttpsNoValidate
  deriving (Show, Eq)

type HasFakeS3Server context = HasLabel context "fakeS3Server" FakeS3Server

fakeS3ServerEndpoint :: FakeS3Server -> Text
fakeS3ServerEndpoint (FakeS3Server {..}) = [i|#{protocol}://#{fakeS3ServerHostname}:#{fakeS3ServerPort}|]
  where protocol :: Text = if fakeS3ServerHttpMode == HttpModeHttp then "http" else "https"

fakeS3ConnectionInfo :: FakeS3Server -> ConnectInfo
fakeS3ConnectionInfo fakeServ@(FakeS3Server {..}) =
  fromString (toString (fakeS3ServerEndpoint fakeServ))
  & setCreds (CredentialValue (AccessKey fakeS3ServerAccessKeyId) (SecretKey (fromString (toString fakeS3ServerSecretAccessKey))) Nothing)
  & (if fakeS3ServerHttpMode == HttpModeHttpsNoValidate then disableTLSCertValidation else id)

data MinIOContextOptions = MinIOContextOptions {
  minioContextBucket :: Maybe Text
  , minioContextLabels :: Map Text Text
  , minioContextContainerName :: Maybe Text
  , minioContextContainerSystem :: ContainerSystem
  } deriving (Show, Eq)
defaultMinIOContextOptions :: MinIOContextOptions
defaultMinIOContextOptions = MinIOContextOptions {
  minioContextBucket = Just "bucket1"
  , minioContextLabels = mempty
  , minioContextContainerName = Nothing
  , minioContextContainerSystem = ContainerSystemPodman
  }

-- * Raw

introduceMinIONix :: (
  HasBaseContext context, HasNixContext context, MonadMask m, MonadUnliftIO m
  ) => MinIOContextOptions
    -> SpecFree (LabelValue "fakeS3Server" FakeS3Server :> LabelValue (AppendSymbol "file-" "minio") (EnvironmentFile "minio") :> context) m ()
    -> SpecFree context m ()
introduceMinIONix options = introduceBinaryViaNixPackage @"minio" "minio" .
  introduceWith "MinIO S3 server" fakeS3Server (withMinIOViaBinary options)

withMinIOViaBinary :: (
  MonadReader context m, HasBaseContext context, HasFile context "minio"
  , MonadLoggerIO m, MonadMask m, MonadUnliftIO m
  ) => MinIOContextOptions -> (FakeS3Server -> m [Result]) -> m ()
withMinIOViaBinary options action = do
  minioPath <- askFile @"minio"
  withMinIO minioPath options action

withMinIO :: (
  MonadReader context m, HasBaseContext context
  , MonadLoggerIO m, MonadMask m, MonadUnliftIO m
  ) => FilePath -> MinIOContextOptions -> (FakeS3Server -> m [Result]) -> m ()
withMinIO minioPath (MinIOContextOptions {..}) action = do
  dir <- getCurrentFolder >>= \case
    Nothing -> expectationFailure "withMinIOViaBinary must be run with a current directory."
    Just x -> return x

  minioDir <- liftIO $ createTempDirectory dir "minio-storage"

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

  withAsync forwardStderr $ \_ ->
    withCreateProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle hWriteErr }) $ \_ (Just sout) _ _p -> do
      uriToUse <- fix $ \loop -> do
        line <- liftIO $ T.hGetLine sout
        debug [i|minio: #{line}|]
        case A.eitherDecode (encodeUtf8 line) of
          Right (A.Object ( aesonLookup "message" -> Just (A.String t) ))
            | "S3-API:" `T.isPrefixOf` t -> do
                return $ t
                       & T.drop (T.length "S3-API:")
                       & T.words
                       & mapMaybe (parseURI . toString)
                       & L.sortOn scoreUri
                       & headMay
            | otherwise -> loop
          _ -> loop

      (hostname, port) <- case uriToUse of
        Nothing -> expectationFailure [i|Couldn't find MinIO URI to use.|]
        Just (URI { uriAuthority=(Just URIAuth {..}) }) -> case readMaybe (L.drop 1 uriPort) of
          Just p -> pure (uriRegName, p)
          Nothing -> expectationFailure [i|Couldn't parse URI port: '#{uriPort}'|]
        Just uri -> expectationFailure [i|MinIO URI didn't have hostname: #{uri}|]

      let server = FakeS3Server {
            fakeS3ServerHostname = hostname
            , fakeS3ServerPort = port
            , fakeS3ServerAccessKeyId = "minioadmin"
            , fakeS3ServerSecretAccessKey = "minioadmin"
            , fakeS3ServerBucket = minioContextBucket
            , fakeS3ServerHttpMode = HttpModeHttp
            }

      waitForMinIOReady server

      void $ action server

  where
    -- URIs will be sorted from low to high according to this key function
    scoreUri :: URI -> Int
    scoreUri (URI { uriAuthority=(Just URIAuth {..}) })
      | uriRegName == "127.0.0.1" = -10
      | uriRegName == "localhost" = -5
    scoreUri _ = 0


-- * Container

introduceMinIOContainer :: (
  HasBaseContext context, MonadMask m, MonadUnliftIO m
  ) => MinIOContextOptions -> SpecFree (LabelValue "fakeS3Server" FakeS3Server :> context) m () -> SpecFree context m ()
introduceMinIOContainer options = introduceWith "MinIO S3 server" fakeS3Server $ \action -> do
  withMinIOContainer options action

withMinIOContainer :: (
  MonadReader context m, HasBaseContext context
  , MonadLoggerIO m, MonadMask m, MonadUnliftIO m
  ) => MinIOContextOptions -> (FakeS3Server -> m [Result]) -> m ()
withMinIOContainer (MinIOContextOptions {..}) action = do
  folder <- getCurrentFolder >>= \case
    Nothing -> expectationFailure "withMinIOContainer must be run with a current directory."
    Just x -> return x

  let mockDir = folder </> "mock_root"
  createDirectoryIfMissing True mockDir
  liftIO $ void $ readCreateProcess (proc "chmod" ["777", mockDir]) "" -- Fix permission problems on GitHub Runners

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
                ExitFailure n -> expectationFailure [i|Failed to start MinIO container (exit code #{n})|]

              localPort <- containerPortToHostPort minioContextContainerSystem containerName innerPort

              let server = FakeS3Server {
                    fakeS3ServerHostname = "127.0.0.1"
                    , fakeS3ServerPort = localPort
                    , fakeS3ServerAccessKeyId = "minioadmin"
                    , fakeS3ServerSecretAccessKey = "minioadmin"
                    , fakeS3ServerBucket = minioContextBucket
                    , fakeS3ServerHttpMode = HttpModeHttp
                    }

              waitForMinIOReady server

              void $ action server
          )


waitForMinIOReady :: (MonadLogger m, MonadUnliftIO m, MonadMask m) => FakeS3Server -> m ()
waitForMinIOReady server@(FakeS3Server {..}) = do
  -- The minio image seems not to have a healthcheck?
  -- waitForHealth containerName
  waitUntilStatusCodeWithTimeout' (1_000_000 * 60 * 5) (2, 0, 0) NoVerify [i|http://#{fakeS3ServerHostname}:#{fakeS3ServerPort}/minio/health/live|]

  whenJust fakeS3ServerBucket $ \bucket -> do
    -- Make the test bucket, retrying on ServiceErr
    let connInfo :: ConnectInfo = setCreds (CredentialValue "minioadmin" "minioadmin" Nothing) [i|http://#{fakeS3ServerHostname}:#{fakeS3ServerPort}|]
    let policy = limitRetriesByCumulativeDelay (1_000_000 * 60 * 5) $ capDelay 1_000_000 $ exponentialBackoff 50_000
    let handlers = [\_ -> MC.Handler (\case (ServiceErr {}) -> return True; _ -> return False)
                   , \_ -> MC.Handler (\case (MErrService (ServiceErr {})) -> return True; _ -> return False)]
    debug [i|Starting to try to make bucket at http://#{fakeS3ServerHostname}:#{fakeS3ServerPort}|]
    recovering policy handlers $ \retryStatus@(RetryStatus {}) -> do
      info [i|About to try making S3 bucket with retry status: #{retryStatus}|]
      liftIO $ doMakeBucket connInfo bucket

  debug [i|MinIO S3 server ready: #{server}|]


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
