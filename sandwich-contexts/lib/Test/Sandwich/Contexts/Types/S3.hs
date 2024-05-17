{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Test.Sandwich.Contexts.Types.S3 where

import Data.String.Interpolate
import Relude
import Test.Sandwich
import Test.Sandwich.Contexts.Types.Network


testS3Server :: Label "testS3Server" TestS3Server
testS3Server = Label

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

data HttpMode = HttpModeHttp | HttpModeHttps | HttpModeHttpsNoValidate
  deriving (Show, Eq)

type HasTestS3Server context = HasLabel context "testS3Server" TestS3Server

testS3ServerEndpoint :: TestS3Server -> Text
testS3ServerEndpoint serv@(TestS3Server {testS3ServerAddress=(NetworkAddressTCP hostname port)}) =
  [i|#{s3Protocol serv}://#{hostname}:#{port}|]
testS3ServerEndpoint serv@(TestS3Server {testS3ServerAddress=(NetworkAddressUnix path)}) =
  [i|#{s3Protocol serv}://#{path}|]

testS3ServerContainerEndpoint :: TestS3Server -> Maybe Text
testS3ServerContainerEndpoint serv@(TestS3Server {testS3ServerContainerAddress=(Just (NetworkAddressTCP hostname port))}) =
  Just [i|#{s3Protocol serv}://#{hostname}:#{port}|]
testS3ServerContainerEndpoint serv@(TestS3Server {testS3ServerContainerAddress=(Just (NetworkAddressUnix path))}) =
  Just [i|#{s3Protocol serv}://#{path}|]
testS3ServerContainerEndpoint _ = Nothing

s3Protocol :: TestS3Server -> Text
s3Protocol (TestS3Server {..}) = if testS3ServerHttpMode == HttpModeHttp then "http" else "https"
