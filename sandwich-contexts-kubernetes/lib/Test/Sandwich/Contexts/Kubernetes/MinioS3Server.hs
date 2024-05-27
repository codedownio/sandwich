{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinioS3Server (
  introduceK8SMinioS3Server
  , introduceK8SMinioS3Server'
  , withK8SMinioS3Server
  , withK8SMinioS3Server'

  -- * Re-exports
  , testS3Server
  , TestS3Server(..)
  , HasTestS3Server
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.ByteString.Base64 as B64
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import Network.Minio
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Cluster
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server.Parsing
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.UUID
import Test.Sandwich.Contexts.MinIO
import Test.Sandwich.Contexts.Waits
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.Process


-- | Introduce a MinIO server on a Kubernetes cluster.
-- Must have a 'minioOperator' context.
introduceK8SMinioS3Server :: (
  MonadMask m, MonadUnliftIO m
  , HasBaseContextMonad context m, HasMinioOperatorContext context, HasKubernetesClusterContext context
  , HasFile context "kubectl", HasFile context "kubectl-minio"
  ) => Text -> SpecFree (LabelValue "testS3Server" TestS3Server :> context) m () -> SpecFree context m ()
introduceK8SMinioS3Server namespace = do
  introduceWith "minio S3 server" testS3Server $ \action -> do
    kcc <- getContext kubernetesCluster
    moc <- getContext minioOperator
    withK8SMinioS3Server kcc moc namespace action

-- | Same as 'introduceK8SMinioS3Server', but allows you to pass in the 'KubernetesClusterContext'.
introduceK8SMinioS3Server' :: (
  MonadMask m, MonadUnliftIO m
  , HasBaseContextMonad context m, HasMinioOperatorContext context, HasFile context "kubectl", HasFile context "kubectl-minio"
  ) => KubernetesClusterContext -> Text -> SpecFree (LabelValue "testS3Server" TestS3Server :> context) m () -> SpecFree context m ()
introduceK8SMinioS3Server' kubernetesClusterContext namespace =
  introduceWith "minio S3 server" testS3Server $ \action -> do
    moc <- getContext minioOperator
    withK8SMinioS3Server kubernetesClusterContext moc namespace action

-- | Bracket-style variant of 'introduceK8SMinioS3Server'.
withK8SMinioS3Server :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m, MonadFail m
  , HasBaseContextMonad context m, HasFile context "kubectl", HasFile context "kubectl-minio"
  ) => KubernetesClusterContext -> MinioOperatorContext -> Text -> (TestS3Server -> m [Result]) -> m ()
withK8SMinioS3Server kcc moc namespace action = do
  kubectlBinary <- askFile @"kubectl"
  kubectlMinioBinary <- askFile @"kubectl-minio"
  withK8SMinioS3Server' kubectlBinary kubectlMinioBinary kcc moc namespace action

withK8SMinioS3Server' :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m, MonadFail m
  , HasBaseContextMonad context m
  ) => FilePath -> FilePath -> KubernetesClusterContext -> MinioOperatorContext -> Text -> (TestS3Server -> m [Result]) -> m ()
withK8SMinioS3Server' kubectlBinary kubectlMinioBinary (KubernetesClusterContext {..}) MinioOperatorContext namespace action = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
  let runWithKubeConfig prog args = do
        p <- createProcessWithLogging ((proc prog args) { env = Just env, delegate_ctlc = True })
        waitForProcess p >>= (`shouldBe` ExitSuccess)

  deploymentName <- ("minio-" <>) <$> makeUUID' 5

  let pool = "pool1"
  let port = 80

  let create = do
        runWithKubeConfig kubectlMinioBinary [
          "tenant", "create", toString deploymentName
          , "--namespace", toString namespace
          , "--servers", "1"
          , "--volumes", "1"
          , "--capacity", "10G"
          , "--pool", pool
          , "--disable-tls"
          ]

  let destroy = do
        runWithKubeConfig kubectlMinioBinary ["tenant", "delete", toString deploymentName
                                             , "--namespace", toString namespace
                                             , "-f"
                                             ]

  bracket_ create destroy $ do
    Right envConfig <- ((B64.decode . encodeUtf8 . T.strip . toText) <$>) $
      readCreateProcess ((proc kubectlBinary ["get", "secret", [i|#{deploymentName}-env-configuration|]
                                              , "--namespace", toString namespace
                                              , "-o", [i|jsonpath="{.data.config\\.env}"|]
                                              ]) { env = Just env }) ""
    info [i|Got envConfig: #{envConfig}|]

    Just (username, password) <- return (parseMinioUserAndPassword (decodeUtf8 envConfig))
    info [i|Got username and password: #{(username, password)}|]

    do
      uuid <- makeUUID
      p <- createProcessWithLogging ((proc kubectlBinary [
                                         "run", "discoverer-" <> toString uuid
                                         , "--rm", "-i"
                                         , "--attach"
                                         , "--image=busybox:1.36.1-musl"
                                         , "--restart=Never"
                                         , "--command"
                                         , "--namespace", toString namespace
                                         , "--"
                                         , "sh", "-c", [i|until nc -vz minio 80; do echo "Waiting for minio..."; sleep 3; done;|]
                                         ]) { env = Just env })
      waitForProcess p >>= (`shouldBe` ExitSuccess)

    info [__i|Ready to try port-forward:
              export KUBECONFIG=#{kubernetesClusterKubeConfigPath}
              kubectl --namespace #{namespace} port-forward "service/minio" 8080:#{port}|]

    withKubectlPortForward' kubectlBinary kubernetesClusterKubeConfigPath namespace (const True) Nothing "service/minio" port $ \(KubectlPortForwardContext {..}) -> do
      info [i|Did forward to localhost:#{kubectlPortForwardPort}|]
      -- liftIO $ threadDelay 999999999999

      let bucket = "bucket1"

      let testServ = TestS3Server {
            testS3ServerAddress = NetworkAddressTCP "localhost" kubectlPortForwardPort
            , testS3ServerContainerAddress = Just $ NetworkAddressTCP "minio" port
            , testS3ServerAccessKeyId = username
            , testS3ServerSecretAccessKey = password
            , testS3ServerBucket = Just bucket
            , testS3ServerHttpMode = HttpModeHttp
            }

      liftIO (runMinio (testS3ServerConnectInfo testServ) $ makeBucket bucket Nothing) >>= \case
        Left err -> expectationFailure [i|Failed to create bucket: #{err}|]
        Right () -> return ()

      waitUntilStatusCodeWithTimeout (4, 0, 3) (1_000_000 * 60 * 5) NoVerify (toString (testS3ServerEndpoint testServ))

      void $ action testServ
