{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-|

Deploy a PostgreSQL server onto a Kubernetes cluster.

The server is provided as a generic 'PostgresContext' (from @sandwich-contexts@), so that you can
easily run the same tests against both Kubernetes environments and host-level ones.

-}

module Test.Sandwich.Contexts.Kubernetes.PostgresServer (
  introduceK8SPostgresServer
  , introduceK8SPostgresServer'

  -- * Bracket-style variants
  , withK8SPostgresServer
  , withK8SPostgresServer'

  -- * Types
  , PostgresK8SOptions(..)
  , defaultPostgresK8SOptions

  -- * Re-exports
  , postgres
  , PostgresContext(..)
  , NetworkAddress(..)
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Data.Text as T
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.UUID
import Test.Sandwich.Contexts.PostgreSQL
import Test.Sandwich.Contexts.Types.Network
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Timeout


data PostgresK8SOptions = PostgresK8SOptions {
  postgresK8SNamespace :: Text
  , postgresK8SImage :: Text
  , postgresK8SUsername :: Text
  , postgresK8SPassword :: Text
  , postgresK8SDatabase :: Text
  , postgresK8SPreloadImage :: Bool
  }
defaultPostgresK8SOptions :: Text -> PostgresK8SOptions
defaultPostgresK8SOptions namespace = PostgresK8SOptions {
  postgresK8SNamespace = namespace
  , postgresK8SImage = "docker.io/postgres:15"
  , postgresK8SUsername = "postgres"
  , postgresK8SPassword = "postgres"
  , postgresK8SDatabase = "test"
  , postgresK8SPreloadImage = True
  }

-- | Introduce a PostgreSQL server on a Kubernetes cluster.
introduceK8SPostgresServer :: (
  Typeable context, KubectlBasicWithoutReader context m
  )
  -- | Options
  => PostgresK8SOptions
  -> SpecFree (LabelValue "postgres" PostgresContext :> context) m ()
  -> SpecFree context m ()
introduceK8SPostgresServer options =
  introduceWith "PostgreSQL on K8S" postgres $ \action -> do
    kcc <- getContext kubernetesCluster
    withK8SPostgresServer kcc options action

-- | Same as 'introduceK8SPostgresServer', but allows you to pass in the 'KubernetesClusterContext'.
introduceK8SPostgresServer' :: (
  Typeable context, KubectlBasic context m
  )
  => KubernetesClusterContext
  -- | Options
  -> PostgresK8SOptions
  -> SpecFree (LabelValue "postgres" PostgresContext :> context) m ()
  -> SpecFree context m ()
introduceK8SPostgresServer' kcc options =
  introduceWith "PostgreSQL on K8S" postgres $ \action ->
    withK8SPostgresServer kcc options action

-- | Bracket-style variant of 'introduceK8SPostgresServer'.
withK8SPostgresServer :: (
  Typeable context, MonadFail m, KubernetesBasic context m, HasFile context "kubectl"
  )
  => KubernetesClusterContext
  -- | Options
  -> PostgresK8SOptions
  -> (PostgresContext -> m [Result])
  -> m ()
withK8SPostgresServer kcc options action = do
  kubectlBinary <- askFile @"kubectl"
  withK8SPostgresServer' kubectlBinary kcc options action

-- | Same as 'withK8SPostgresServer', but allows you to pass in the @kubectl@ binary.
withK8SPostgresServer' :: forall m context. (
  Typeable context, MonadFail m, KubernetesBasic context m
  )
  -- | Path to kubectl binary
  => FilePath
  -> KubernetesClusterContext
  -- | Options
  -> PostgresK8SOptions
  -> (PostgresContext -> m [Result])
  -> m ()
withK8SPostgresServer' kubectlBinary kcc@(KubernetesClusterContext {..}) (PostgresK8SOptions {..}) action = do
  env <- getKubectlEnvironment kcc

  deploymentName <- ("postgres-" <>) <$> makeUUID' 5

  let postgresPort = 5432 :: Int

  when postgresK8SPreloadImage $ do
    debug [i|Preloading postgres image: #{postgresK8SImage}|]
    loadImageIfNecessary' kcc (ImageLoadSpecDocker postgresK8SImage IfNotPresent)

  let yaml = postgresYaml deploymentName postgresK8SNamespace postgresK8SImage
                          postgresK8SUsername postgresK8SPassword postgresK8SDatabase

  let create = do
        (ps, _) <- createProcessWithLoggingAndStdin
          ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env })
          (toString yaml)
        waitForProcess ps >>= (`shouldBe` ExitSuccess)

  let destroy = do
        info [i|Destroying PostgreSQL K8S resources|]
        (ps, _) <- createProcessWithLoggingAndStdin
          ((proc kubectlBinary ["delete", "-f", "-", "--ignore-not-found"]) { env = Just env })
          (toString yaml)
        waitForProcess ps >>= (`shouldBe` ExitSuccess)

  bracket_ create destroy $ do
    -- Wait for the pod to be ready
    let waitArgs = ["wait", "pod"
                   , "-l", [i|app=#{deploymentName}|]
                   , "--namespace", toString postgresK8SNamespace
                   , "--for", "condition=Ready"
                   , "--timeout=120s"
                   , "--kubeconfig", kubernetesClusterKubeConfigPath
                   ]
    timeout 150_000_000 (do
      (ps, _) <- createProcessWithLogging ((proc kubectlBinary waitArgs) { env = Just env })
      waitForProcess ps >>= (`shouldBe` ExitSuccess)
      ) >>= \case
        Just () -> return ()
        Nothing -> expectationFailure [i|Timed out waiting for PostgreSQL pod to be ready|]

    let serviceName = [i|service/#{deploymentName}|] :: Text
    withKubectlPortForward' kubectlBinary kubernetesClusterKubeConfigPath postgresK8SNamespace
                            (const True) Nothing serviceName (fromIntegral postgresPort) $ \(KubectlPortForwardContext {..}) -> do
      info [i|PostgreSQL port-forwarded to localhost:#{kubectlPortForwardPort}|]

      let ctx = PostgresContext {
            postgresUsername = postgresK8SUsername
            , postgresPassword = postgresK8SPassword
            , postgresDatabase = postgresK8SDatabase
            , postgresAddress = NetworkAddressTCP "localhost" kubectlPortForwardPort
            , postgresConnString = [i|postgresql://#{postgresK8SUsername}:#{postgresK8SPassword}@localhost:#{kubectlPortForwardPort}/#{postgresK8SDatabase}|]
            , postgresContainerAddress = Just $ NetworkAddressTCP (toString deploymentName) (fromIntegral postgresPort)
            }

      void $ action ctx


postgresYaml :: Text -> Text -> Text -> Text -> Text -> Text -> Text
postgresYaml name namespace image username password database = [__i|
  apiVersion: v1
  kind: Pod
  metadata:
    name: #{name}
    namespace: #{namespace}
    labels:
      app: "#{name}"
  spec:
    containers:
    - name: postgres
      image: "#{image}"
      imagePullPolicy: IfNotPresent
      env:
      - name: POSTGRES_USER
        value: "#{username}"
      - name: POSTGRES_PASSWORD
        value: "#{password}"
      - name: POSTGRES_DB
        value: "#{database}"
      ports:
      - containerPort: 5432
      readinessProbe:
        exec:
          command:
          - pg_isready
          - -U
          - "#{username}"
        initialDelaySeconds: 5
        periodSeconds: 2
  ---
  apiVersion: v1
  kind: Service
  metadata:
    name: #{name}
    namespace: #{namespace}
  spec:
    selector:
      app: "#{name}"
    ports:
    - port: 5432
      targetPort: 5432
  |]
