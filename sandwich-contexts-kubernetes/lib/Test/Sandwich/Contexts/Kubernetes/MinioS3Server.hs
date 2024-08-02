{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.MinioS3Server (
  introduceK8SMinioS3Server
  , introduceK8SMinioS3Server'
  , withK8SMinioS3Server
  , withK8SMinioS3Server'

  , MinioS3ServerOptions(..)
  , defaultMinioS3ServerOptions

  -- * Re-exports
  , testS3Server
  , TestS3Server(..)
  , HasTestS3Server
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Text as T
import Network.Minio
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Cluster
import Test.Sandwich.Contexts.Kubernetes.FindImages
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.MinioS3Server.Parsing
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.UUID
import Test.Sandwich.Contexts.MinIO
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Timeout


data MinioS3ServerOptions = MinioS3ServerOptions {
  minioS3ServerNamespace :: Text
  , minioS3ServerKustomizationDir :: KustomizationDir
  , minioS3ServerPreloadImages :: Bool
  }
defaultMinioS3ServerOptions :: Text -> MinioS3ServerOptions
defaultMinioS3ServerOptions namespace = MinioS3ServerOptions {
  minioS3ServerNamespace = namespace
  , minioS3ServerKustomizationDir = KustomizationDirUrl "https://github.com/minio/operator/examples/kustomization/base?ref=v6.0.1"
  , minioS3ServerPreloadImages = True
  }

data KustomizationDir =
  -- | URL Kustomize dir to be downloaded
  KustomizationDirUrl Text
  -- | Local Kustomize dir
  | KustomizationDirLocal FilePath
  -- | A Nix callPackage-style derivation to produce the Kustomize dir
  | KustomizationDirNixDerivation Text
  deriving (Show, Eq)

-- | Introduce a MinIO server on a Kubernetes cluster.
-- Must have a 'minioOperator' context.
introduceK8SMinioS3Server :: (
  MonadMask m, MonadUnliftIO m, Typeable context
  , HasBaseContext context, HasMinioOperatorContext context, HasKubernetesClusterContext context
  , HasFile context "kubectl"
  )
  -- | Options
  => MinioS3ServerOptions
  -> SpecFree (LabelValue "testS3Server" TestS3Server :> context) m ()
  -> SpecFree context m ()
introduceK8SMinioS3Server options = do
  introduceWith "minio S3 server" testS3Server $ \action -> do
    kcc <- getContext kubernetesCluster
    moc <- getContext minioOperator
    withK8SMinioS3Server kcc moc options action

-- | Same as 'introduceK8SMinioS3Server', but allows you to pass in the 'KubernetesClusterContext'.
introduceK8SMinioS3Server' :: (
  MonadMask m, MonadUnliftIO m, Typeable context
  , HasBaseContext context, HasMinioOperatorContext context, HasFile context "kubectl"
  )
  => KubernetesClusterContext
  -- | Options
  -> MinioS3ServerOptions
  -> SpecFree (LabelValue "testS3Server" TestS3Server :> context) m ()
  -> SpecFree context m ()
introduceK8SMinioS3Server' kubernetesClusterContext options =
  introduceWith "minio S3 server" testS3Server $ \action -> do
    moc <- getContext minioOperator
    withK8SMinioS3Server kubernetesClusterContext moc options action

-- | Bracket-style variant of 'introduceK8SMinioS3Server'.
withK8SMinioS3Server :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m, MonadFail m
  , HasBaseContextMonad context m, HasFile context "kubectl", Typeable context
  )
  => KubernetesClusterContext
  -> MinioOperatorContext
  -- | Options
  -> MinioS3ServerOptions
  -> (TestS3Server -> m [Result])
  -> m ()
withK8SMinioS3Server kcc moc options action = do
  kubectlBinary <- askFile @"kubectl"
  withK8SMinioS3Server' kubectlBinary kcc moc options action

-- | Same as 'withK8SMinioS3Server', but allows you to pass in the kubectl and kubectl-minio binaries.
withK8SMinioS3Server' :: (
  MonadLoggerIO m, MonadMask m, MonadUnliftIO m, MonadFail m
  , HasBaseContextMonad context m, Typeable context
  )
  -- | Path to kubectl binary
  => FilePath
  -> KubernetesClusterContext
  -> MinioOperatorContext
  -- | Options
  -> MinioS3ServerOptions
  -> (TestS3Server -> m [Result])
  -> m ()
withK8SMinioS3Server' kubectlBinary kcc@(KubernetesClusterContext {..}) MinioOperatorContext (MinioS3ServerOptions {..}) action = do
  (_, env) <- runWithKubectl' kcc kubectlBinary
  let runWithKubeConfig prog args = do
        createProcessWithLogging ((proc prog args) { env = Just env, delegate_ctlc = True })
          >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  deploymentName <- ("minio-" <>) <$> makeUUID' 5

  -- let pool = "pool1"
  let port = 80

  kustomizationDir <- case minioS3ServerKustomizationDir of
    KustomizationDirLocal p -> pure p
    KustomizationDirUrl u -> pure (toString u)
    KustomizationDirNixDerivation d -> do
      getContextMaybe nixContext >>= \case
        Nothing -> expectationFailure [i|Couldn't find a Nix context to use with KustomizationDirNixDerivation|]
        Just nc -> buildNixCallPackageDerivation' nc d

  let busyboxImage = "busybox:1.36.1-musl"

  let create = do
        allYaml <- readCreateProcessWithLogging ((proc kubectlBinary ["kustomize", kustomizationDir]) { env = Just env, delegate_ctlc = True }) ""

        when minioS3ServerPreloadImages $ do
          let images = findAllImages (toText allYaml)

          forM_ images $ \image ->
            loadImageIfNecessary' kcc (ImageLoadSpecDocker image IfNotPresent)

          loadImageIfNecessary' kcc (ImageLoadSpecDocker busyboxImage IfNotPresent)

        (userAndPassword@(username, password), finalYaml) <- case transformKustomizeChunks (toString minioS3ServerNamespace) (T.splitOn "---\n" (toText allYaml)) of
          Left err -> expectationFailure [i|Couldn't transform kustomize chunks: #{err}|]
          Right x -> pure x

        info [i|Got username and password: #{(username, password)}|]

        createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString finalYaml)
          >>= waitForProcess >>= (`shouldBe` ExitSuccess)

        return userAndPassword

  let destroy _ =
        runWithKubeConfig kubectlBinary ["delete", "-k", kustomizationDir
                                        , "--namespace", toString minioS3ServerNamespace
                                        , "-f"
                                        ]

  let createNetworkPolicy = do
        let (policyName, discoverPodPolicyName, yaml) = networkPolicy deploymentName
        createProcessWithLoggingAndStdin ((proc kubectlBinary ["create", "--namespace", toString minioS3ServerNamespace, "-f", "-"]) { env = Just env, delegate_ctlc = True }) yaml
          >>= waitForProcess >>= (`shouldBe` ExitSuccess)
        pure (policyName, discoverPodPolicyName)
  let destroyNetworkPolicy (policyName, discoverPodPolicyName) = do
        runWithKubeConfig kubectlBinary ["delete", "NetworkPolicy", policyName, "--namespace", toString minioS3ServerNamespace]
        runWithKubeConfig kubectlBinary ["delete", "NetworkPolicy", discoverPodPolicyName, "--namespace", toString minioS3ServerNamespace]

  -- TODO: create network policy allowing ingress/egress for v1.min.io/tenant = deploymentName
  bracket createNetworkPolicy destroyNetworkPolicy $ \_ -> bracket create destroy $ \(username, password) -> do
    do
      uuid <- makeUUID
      p <- createProcessWithLogging ((proc kubectlBinary [
                                         "run", "discoverer-" <> toString uuid
                                         , "--rm", "-i"
                                         , "--attach"
                                         , [i|--image=#{busyboxImage}|]
                                         , "--restart=Never"
                                         , "--command"
                                         , "--namespace", toString minioS3ServerNamespace
                                         , "--labels=app=discover-pod"
                                         , "--"
                                         , "sh", "-c", [i|until nc -vz minio 80; do echo "Waiting for minio..."; sleep 3; done;|]
                                         ]) { env = Just env })
      timeout 300_000_000 (waitForProcess p >>= (`shouldBe` ExitSuccess)) >>= \case
        Just () -> return ()
        Nothing -> expectationFailure [i|Failed to wait for minio to come online.|]

    info [__i|Ready to try port-forward:
              export KUBECONFIG=#{kubernetesClusterKubeConfigPath}
              kubectl --namespace #{minioS3ServerNamespace} port-forward "service/minio" 8080:#{port}|]

    withKubectlPortForward' kubectlBinary kubernetesClusterKubeConfigPath minioS3ServerNamespace (const True) Nothing "service/minio" port $ \(KubectlPortForwardContext {..}) -> do
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


networkPolicy :: Text -> (String, String, String)
networkPolicy deploymentName = (policyName, discoverPodPolicyName, yaml)
  where
    policyName = "minio-allow"
    discoverPodPolicyName = "discover-pod-allow"

    yaml = [__i|apiVersion: networking.k8s.io/v1
                kind: NetworkPolicy
                metadata:
                  name: #{policyName}
                spec:
                  podSelector:
                    matchLabels:
                      v1.min.io/tenant: "#{deploymentName}"

                  policyTypes:
                  - Ingress
                  - Egress

                  ingress:
                  - {}

                  egress:
                  - {}
                ---
                apiVersion: networking.k8s.io/v1
                kind: NetworkPolicy
                metadata:
                  name: #{discoverPodPolicyName}
                spec:
                  podSelector:
                    matchLabels:
                      app: discover-pod

                  policyTypes:
                  - Ingress
                  - Egress

                  ingress:
                  - {}

                  egress:
                  - {}
                |]
