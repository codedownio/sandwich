{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

{-|
Helper module for working with S3 servers.
-}

module Test.Sandwich.Contexts.Types.S3 (
  TestS3Server(..)
  , HttpMode(..)

  -- * Contexts
  , testS3Server
  , HasTestS3Server

  -- * Endpoints
  , testS3ServerEndpoint
  , testS3ServerContainerEndpoint

  -- * Misc
  , s3Protocol
  ) where

import Data.String.Interpolate
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Types.Network


testS3Server :: Label "testS3Server" TestS3Server
testS3Server = Label

-- | A generic test S3 server. This can be used by downstream packages like sandwich-contexts-minio.
data TestS3Server = TestS3Server {
  testS3ServerAddress :: NetworkAddress
  -- | The address of the S3 server within its container, if present.
  -- Useful if you're doing container-to-container networking.
  , testS3ServerContainerAddress :: Maybe NetworkAddress
  , testS3ServerAccessKeyId :: Text
  , testS3ServerSecretAccessKey :: Text
  , testS3ServerBucket :: Maybe Text
  , testS3ServerHttpMode :: HttpMode
  } deriving (Show, Eq)

data HttpMode =
  HttpModeHttp
  | HttpModeHttps
  -- | A special mode to allow a server to run in HTTPS mode, but connect to it without HTTPS validation. Useful for tests.
  | HttpModeHttpsNoValidate
  deriving (Show, Eq)

type HasTestS3Server context = HasLabel context "testS3Server" TestS3Server

-- | Generate an S3 connection string for the given server.
testS3ServerEndpoint :: TestS3Server -> Text
testS3ServerEndpoint serv@(TestS3Server {testS3ServerAddress=(NetworkAddressTCP hostname port)}) =
  [i|#{s3Protocol serv}://#{hostname}:#{port}|]
testS3ServerEndpoint serv@(TestS3Server {testS3ServerAddress=(NetworkAddressUnix path)}) =
  [i|#{s3Protocol serv}://#{path}|]

-- | Generate an S3 connection string for the given containerized server, for the network address inside the container.
-- Returns 'Nothing' if this server isn't containerized.
testS3ServerContainerEndpoint :: TestS3Server -> Maybe Text
testS3ServerContainerEndpoint serv@(TestS3Server {testS3ServerContainerAddress=(Just (NetworkAddressTCP hostname port))}) =
  Just [i|#{s3Protocol serv}://#{hostname}:#{port}|]
testS3ServerContainerEndpoint serv@(TestS3Server {testS3ServerContainerAddress=(Just (NetworkAddressUnix path))}) =
  Just [i|#{s3Protocol serv}://#{path}|]
testS3ServerContainerEndpoint _ = Nothing

-- | Return either "http" or "https", based on the 'testS3ServerHttpMode'.
s3Protocol :: TestS3Server -> Text
s3Protocol (TestS3Server {..}) = if testS3ServerHttpMode == HttpModeHttp then "http" else "https"
