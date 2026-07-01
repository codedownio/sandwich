{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|

Install [SeaweedFS](https://github.com/seaweedfs/seaweedfs) deployments on a Kubernetes cluster.

-}

module Test.Sandwich.Contexts.Kubernetes.SeaweedFS (
  introduceSeaweedFS

  -- * Bracket-style variants
  , withSeaweedFS
  , withSeaweedFS'

  -- * Options
  , defaultSeaweedFSOptions
  , fastSeaweedFSOptions

  -- * Other options
  , defaultSeaweedFSCsiDriverOptions
  , defaultSeaweedFSS3Options
  , defaultSeaweedFsOperatorImageExpr
  , defaultSeaweedFSProbeOverride

  -- * Types
  , SeaweedFSOptions(..)
  , SeaweedFSCsiDriverOptions(..)
  , SeaweedFSS3Options(..)
  , SeaweedFSProbeOverride(..)

  , seaweedFs
  , SeaweedFSContext(..)
  , HasSeaweedFSContext

  -- * Shared internals (used by "Test.Sandwich.Contexts.Kubernetes.SeaweedFSMini")
  , ContextWithSeaweedFS
  , seaweedFsStorageClassName
  , s3ConfigSecretYaml
  , installSeaweedFsCsiDriver
  , retryKubectl
  ) where

import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson as A
import qualified Data.List as L
import qualified Data.Map as Map
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml as Yaml
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Images (loadImage', loadImageIfNecessary')
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import Test.Sandwich.Contexts.Nix
import qualified Toml
import Toml (Table'(MkTable), Value'(..))
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment
import UnliftIO.Exception (handleAny, throwIO)
import UnliftIO.IO (withFile)
import UnliftIO.Process


data SeaweedFSContext = SeaweedFSContext {
  seaweedFsOptions :: SeaweedFSOptions
  -- | @StorageClass@ for SeaweedFS-backed volumes, if the CSI driver was installed (see
  -- 'seaweedFsCsiDriver').
  , seaweedFsCsiStorageClass :: Maybe Text
  -- | Filer JWT write signing key (security.toml @jwt.filer_signing.key@), read from the
  -- @\<baseName\>-security-config@ Secret; present it to call the filer HTTP API directly.
  -- 'Nothing' if absent.
  , seaweedFsFilerJwtWriteKey :: Maybe Text
  -- | Filer JWT read signing key. Always 'Nothing': the pinned operator doesn't emit one.
  , seaweedFsFilerJwtReadKey :: Maybe Text
  } deriving (Show)

data SeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage :: ImageLoadSpec
  -- | Extra images to preload into the cluster (so operator-managed pods don't pull them at
  -- runtime). Defaults to @[]@.
  , seaweedFsExtraImages :: [ImageLoadSpec]
  -- | The Nix expression (built via the test's nix context) producing the operator manager image
  -- tarball. Defaults to 'defaultSeaweedFsOperatorImageExpr'.
  , seaweedFsOperatorImageExpr :: Text
  , seaweedFsBaseName :: Text
  , seaweedFsMasterReplicas :: Int
  , seaweedFsFilerReplicas :: Int
  , seaweedFsVolumeReplicas :: Int
  -- | Set @master.concurrentStart: true@, so the operator creates the
  -- master\/volume\/filer StatefulSets in one pass instead of gating each on
  -- the previous. Cuts startup time; the operator marks it testing-only.
  -- Defaults to 'False' (ordered startup).
  , seaweedFsMasterConcurrentStart :: Bool
  , seaweedFsServerExtraArgs :: [Text]
  -- | Raw @filer.toml@ for the filer metadata store, dropped verbatim into the Seaweed CR's
  -- @spec.filer.config@. 'Nothing' uses the operator default (embedded @leveldb2@). Supply a
  -- @[leveldb3]@\/@[postgres]@\/@[redis2]@ etc. block to select another store; for external
  -- stores you must run the backing DB and (for SQL) create the table yourself.
  , seaweedFsFilerConfig :: Maybe Text
  , seaweedFsMasterReadinessProbe :: Maybe SeaweedFSProbeOverride
  , seaweedFsVolumeReadinessProbe :: Maybe SeaweedFSProbeOverride
  , seaweedFsFilerReadinessProbe :: Maybe SeaweedFSProbeOverride
  , seaweedFsMasterLivenessProbe :: Maybe SeaweedFSProbeOverride
  , seaweedFsVolumeLivenessProbe :: Maybe SeaweedFSProbeOverride
  , seaweedFsFilerLivenessProbe :: Maybe SeaweedFSProbeOverride
  , seaweedFsVolumeServerDiskCount :: Int
  , seaweedFsVolumeSizeLimitMb :: Int
  , seaweedFsVolumeStorageRequest :: Text
  -- | If 'Just', deploy a standalone S3 gateway (@spec.s3@ in the Seaweed CR).
  -- Defaults to 'Just' 'defaultSeaweedFSS3Options'.
  , seaweedFsS3 :: Maybe SeaweedFSS3Options
  -- | Whether to install the
  -- [seaweedfs-csi-driver](https://github.com/seaweedfs/seaweedfs-csi-driver),
  -- which provisions Kubernetes @PersistentVolume@s backed by this SeaweedFS
  -- filer (via the @seaweedfs-storage@ 'StorageClass'). Defaults to 'Just'
  -- 'defaultSeaweedFSCsiDriverOptions'.
  , seaweedFsCsiDriver :: Maybe SeaweedFSCsiDriverOptions
  } deriving (Show)
-- | Default options: 3 master replicas, 2 filer replicas, 1 volume replica.
defaultSeaweedFSOptions :: SeaweedFSOptions
defaultSeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage = ImageLoadSpecDocker "chrislusf/seaweedfs:4.33" IfNotPresent
  , seaweedFsExtraImages = []
  , seaweedFsOperatorImageExpr = defaultSeaweedFsOperatorImageExpr
  , seaweedFsBaseName = "seaweed1"
  , seaweedFsMasterReplicas = 3
  , seaweedFsFilerReplicas = 2
  , seaweedFsVolumeReplicas = 1
  , seaweedFsMasterConcurrentStart = False
  , seaweedFsServerExtraArgs = []
  , seaweedFsFilerConfig = Nothing
  , seaweedFsMasterReadinessProbe = Nothing
  , seaweedFsVolumeReadinessProbe = Nothing
  , seaweedFsFilerReadinessProbe = Nothing
  , seaweedFsMasterLivenessProbe = Nothing
  , seaweedFsVolumeLivenessProbe = Nothing
  , seaweedFsFilerLivenessProbe = Nothing
  , seaweedFsVolumeServerDiskCount = 1
  , seaweedFsVolumeSizeLimitMb = 1024
  , seaweedFsVolumeStorageRequest = "2Gi"
  , seaweedFsS3 = Just defaultSeaweedFSS3Options
  , seaweedFsCsiDriver = Just defaultSeaweedFSCsiDriverOptions
  }

-- | Single-node SeaweedFS options tuned for fast startup on ephemeral test
-- clusters: one replica each, parallel bring-up, @-ip.bind=0.0.0.0@ to avoid
-- the startup DNS crash-loop, and short readiness probes.
fastSeaweedFSOptions :: SeaweedFSOptions
fastSeaweedFSOptions = defaultSeaweedFSOptions {
  seaweedFsMasterReplicas = 1
  , seaweedFsFilerReplicas = 1
  , seaweedFsVolumeReplicas = 1
  , seaweedFsMasterConcurrentStart = True
  , seaweedFsServerExtraArgs = ["-ip.bind=0.0.0.0"]
  , seaweedFsMasterReadinessProbe = Just fastReadinessProbe
  , seaweedFsVolumeReadinessProbe = Just fastReadinessProbe
  , seaweedFsFilerReadinessProbe = Just fastReadinessProbe
  }
  where
    fastReadinessProbe = defaultSeaweedFSProbeOverride {
      seaweedFsProbeInitialDelaySeconds = Just 2
      , seaweedFsProbePeriodSeconds = Just 2
      , seaweedFsProbeSuccessThreshold = Just 1
      }

data SeaweedFSProbeOverride = SeaweedFSProbeOverride {
  seaweedFsProbeInitialDelaySeconds :: Maybe Int
  , seaweedFsProbeTimeoutSeconds :: Maybe Int
  , seaweedFsProbePeriodSeconds :: Maybe Int
  , seaweedFsProbeSuccessThreshold :: Maybe Int
  , seaweedFsProbeFailureThreshold :: Maybe Int
  } deriving (Show, Eq)

defaultSeaweedFSProbeOverride :: SeaweedFSProbeOverride
defaultSeaweedFSProbeOverride = SeaweedFSProbeOverride {
  seaweedFsProbeInitialDelaySeconds = Nothing
  , seaweedFsProbeTimeoutSeconds = Nothing
  , seaweedFsProbePeriodSeconds = Nothing
  , seaweedFsProbeSuccessThreshold = Nothing
  , seaweedFsProbeFailureThreshold = Nothing
  }

-- | Options for installing the SeaweedFS CSI driver.
data SeaweedFSCsiDriverOptions = SeaweedFSCsiDriverOptions {
  -- | The seaweedfs-csi-driver release (git tag) whose @deploy/kubernetes/seaweedfs-csi.yaml@
  -- manifest to install, e.g. @\"v1.4.5\"@.
  seaweedFsCsiDriverVersion :: Text
  -- | Images referenced by the CSI driver manifest, preloaded into the cluster
  -- before applying it (the manifest pins @imagePullPolicy: IfNotPresent@, so
  -- preloading avoids any runtime pull). Must match
  -- 'seaweedFsCsiDriverVersion'.
  , seaweedFsCsiDriverImages :: [ImageLoadSpec]
  } deriving (Show, Eq)

defaultSeaweedFSCsiDriverOptions :: SeaweedFSCsiDriverOptions
defaultSeaweedFSCsiDriverOptions = SeaweedFSCsiDriverOptions {
  seaweedFsCsiDriverVersion = "v1.4.5"
  , seaweedFsCsiDriverImages = fmap (`ImageLoadSpecDocker` IfNotPresent) [
      "chrislusf/seaweedfs-csi-driver:latest"
      , "chrislusf/seaweedfs-mount:latest"
      , "registry.k8s.io/sig-storage/csi-node-driver-registrar:v2.8.0"
      , "registry.k8s.io/sig-storage/csi-provisioner:v3.5.0"
      , "registry.k8s.io/sig-storage/csi-attacher:v4.3.0"
      , "registry.k8s.io/sig-storage/csi-resizer:v1.8.0"
      , "registry.k8s.io/sig-storage/livenessprobe:v2.10.0"
      ]
  }

seaweedFsStorageClassName :: Text
seaweedFsStorageClassName = "seaweedfs-storage"

-- | Options for the standalone S3 gateway (see 'seaweedFsS3'). The gateway is provisioned with a
-- single admin identity using these credentials; consumers that talk to the gateway authenticate
-- with them.
data SeaweedFSS3Options = SeaweedFSS3Options {
  seaweedFsS3AccessKey :: Text
  , seaweedFsS3SecretKey :: Text
  } deriving (Show, Eq)

defaultSeaweedFSS3Options :: SeaweedFSS3Options
defaultSeaweedFSS3Options = SeaweedFSS3Options {
  seaweedFsS3AccessKey = "seaweedfs"
  , seaweedFsS3SecretKey = "seaweedfssecretkey"
  }

-- | The seaweedfs-csi-driver manifest deploys into the @default@ namespace (its RBAC
-- subjects are hardcoded there), regardless of where the SeaweedFS filer lives.
seaweedFsCsiNamespace :: String
seaweedFsCsiNamespace = "default"

seaweedFs :: Label "seaweedFs" SeaweedFSContext
seaweedFs = Label
type HasSeaweedFSContext context = HasLabel context "seaweedFs" SeaweedFSContext

type ContextWithSeaweedFS context =
  LabelValue "seaweedFs" SeaweedFSContext
  :> LabelValue "file-kubectl" (EnvironmentFile "kubectl")
  :> context

-- | Introduce [SeaweedFS](https://github.com/seaweedfs/seaweedfs) on the Kubernetes cluster, in a given namespace.
introduceSeaweedFS :: (
  KubernetesClusterBasicWithoutReader context m, HasNixContext context
  )
  -- | Namespace
  => Text
  -> SeaweedFSOptions
  -> SpecFree (ContextWithSeaweedFS context) m ()
  -> SpecFree context m ()
introduceSeaweedFS namespace options = introduceBinaryViaNixPackage @"kubectl" "kubectl" . introduceWith "introduce SeaweedFS" seaweedFs (void . withSeaweedFS namespace options)

-- | Bracket-style version of 'introduceSeaweedFS'.
withSeaweedFS :: forall context m a. (
  HasCallStack, MonadFail m, KubectlBasic context m, HasNixContext context
  )
  -- | Namespace
  => Text
  -> SeaweedFSOptions
  -> (SeaweedFSContext -> m a)
  -> m a
withSeaweedFS namespace options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withSeaweedFS' kcc kubectlBinary namespace options action

-- | Same as 'withSeaweedFS', but allows you to pass in the 'KubernetesClusterContext' and @kubectl@ binary path.
withSeaweedFS' :: forall context m a. (
  HasCallStack, MonadFail m, NixContextBasic context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -- | Namespace
  -> Text
  -> SeaweedFSOptions
  -> (SeaweedFSContext -> m a)
  -> m a
withSeaweedFS' kcc@(KubernetesClusterContext {kubernetesClusterKubeConfigPath}) kubectlBinary namespace options action = do
  baseEnv <- getEnvironment

  NixContext {..} <- getContext nixContext
  -- Reuse the test's nix context for the nixpkgs pin, rather than pinning our own.
  let renderWithPkgs = renderDerivationWithPkgs nixContextNixpkgsDerivation

  let env = baseEnv
          & (("KUBECONFIG", kubernetesClusterKubeConfigPath) :)
          & L.nubBy (\x y -> fst x == fst y)

  let runK name args = createProcessWithFileLogging' name ((proc kubectlBinary args) { env = Just env })
                       >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  (storageClass, jwtWriteKey, jwtReadKey) <- timeAction "SeaweedFS setup" $ do
    -- The operator manager image, built entirely by Nix (see 'seaweedFsOperatorImageExpr').
    info [i|------------------ Building and loading SeaweedFS operator image ------------------|]
    operatorImageTarball <- timeAction "Build operator image (Nix)" $
      nixBuildExprToPath nixContextNixBinary (toString (renderWithPkgs (seaweedFsOperatorImageExpr options)))
    operatorImageName <- timeAction "Load operator image into cluster" $
      loadImage' kcc (ImageLoadSpecTarball (toString operatorImageTarball))
    info [i|Loaded operator image into cluster as: #{operatorImageName}|]

    info [i|------------------ Preloading SeaweedFS server image ------------------|]
    -- Load conditionally so a baked-in image (e.g. from a minikube preload) is skipped; either way we
    -- get back the ref to configure the SeaweedFS CR with.
    imageName <- timeAction "Preload SeaweedFS server image" $
      loadImageIfNecessary' kcc (seaweedFsImage options)

    info [i|------------------ Preloading extra images ------------------|]
    timeAction "Preload extra images" $
      forM_ (seaweedFsExtraImages options) $ \spec -> do
        loaded <- loadImage' kcc spec
        info [i|Preloaded extra image: #{loaded}|]

    -- The operator's CRD + controller manifests, rendered by Nix via kustomize
    info [i|------------------ Installing SeaweedFS operator ------------------|]
    manifestsDir <- timeAction "Render operator manifests (Nix)" $
      nixBuildExprToPath nixContextNixBinary (toString (renderWithPkgs seaweedFsOperatorManifestsExpr))
    timeAction "Apply operator manifests" $ do
      runK "seaweedfs-operator-crds" ["apply", "--server-side", "-f", toString manifestsDir </> "crd.yaml"]
      runK "seaweedfs-operator-deploy" ["apply", "-f", toString manifestsDir </> "operator.yaml"]
    timeAction "Wait for operator controller-manager" $
      runK "seaweedfs-operator-wait" [
        "wait", "deployment/seaweedfs-operator-controller-manager", "-n", "seaweedfs-operator-system"
        , "--for", "condition=Available", "--timeout=300s"
        ]

    -- The S3 gateway mounts this secret as its identities config; create it before the CR so the
    -- gateway pod can start. Done with @apply@ so a re-run against an existing namespace is fine.
    whenJust (seaweedFsS3 options) $ \s3Options -> timeAction "Create S3 config secret" $
      createProcessWithFileLoggingAndStdin' "seaweedfs-s3-secret" ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString (s3ConfigSecretYaml namespace (seaweedFsBaseName options) s3Options))
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    info [i|------------------ Creating SeaweedFS deployment ------------------|]
    timeAction "Create Seaweed CR" $ do
      let val = decodeUtf8 $ A.encode $ example namespace imageName options
      createProcessWithFileLoggingAndStdin' "seaweedfs-kubectl-create" ((proc kubectlBinary ["create", "-f", "-"]) { env = Just env }) val
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    timeAction "Wait for SeaweedFS cluster (master/volume/filer)" $ do
      info [i|Waiting for the SeaweedFS cluster (master/volume/filer) to be ready|]
      forM_ (["master", "volume", "filer"] :: [Text]) $ \component ->
        retryKubectl kubectlBinary env (toString ([i|seaweedfs-wait-#{component}|] :: Text))
          ["rollout", "status", "-n", toString namespace, toString ([i|statefulset/#{seaweedFsBaseName options}-#{component}|] :: Text), "--timeout=300s"]
      -- The standalone S3 gateway is a stateless Deployment (<baseName>-s3), not a StatefulSet.
      when (isJust (seaweedFsS3 options)) $
        retryKubectl kubectlBinary env "seaweedfs-wait-s3"
          ["rollout", "status", "-n", toString namespace, toString ([i|deployment/#{seaweedFsBaseName options}-s3|] :: Text), "--timeout=300s"]

    storageClass <- case seaweedFsCsiDriver options of
      Nothing -> do
        info [i|Skipping SeaweedFS CSI driver installation|]
        pure Nothing
      Just csiOptions -> timeAction "Install CSI driver" $ do
        info [i|------------------ Preloading SeaweedFS CSI driver images ------------------|]
        forM_ (seaweedFsCsiDriverImages csiOptions) $ \spec -> do
          loaded <- loadImage' kcc spec
          info [i|Preloaded CSI image: #{loaded}|]
        info [i|------------------ Installing SeaweedFS CSI driver ------------------|]
        installSeaweedFsCsiDriver nixContextNixBinary nixContextNixpkgsDerivation kubectlBinary env namespace (seaweedFsBaseName options) csiOptions
        pure (Just seaweedFsStorageClassName)

    -- Read the operator's security.toml (a base64-encoded @Secret@) so callers can present the
    -- @jwt.filer_signing.key@ write key to the filer HTTP API. No read key is emitted, so it stays
    -- 'Nothing'.
    (jwtWriteKey, jwtReadKey) <- do
      let secretName = seaweedFsBaseName options <> "-security-config"
      securityToml <- handleAny (\_ -> pure "") $ readCreateProcess
        ((proc kubectlBinary
          [ "get", "secret", toString secretName, "-n", toString namespace
          , "-o", "go-template={{ index .data \"security.toml\" | base64decode }}" ]) { env = Just env }) ""
      let keys = parseFilerJwtKeys (T.pack securityToml)
      info [i|SeaweedFS filer JWT keys present: write=#{isJust (fst keys)} read=#{isJust (snd keys)}|]
      pure keys

    return (storageClass, jwtWriteKey, jwtReadKey)

  action $ SeaweedFSContext {
    seaweedFsOptions = options
    , seaweedFsCsiStorageClass = storageClass
    , seaweedFsFilerJwtWriteKey = jwtWriteKey
    , seaweedFsFilerJwtReadKey = jwtReadKey
    }


example :: Text -> Text -> SeaweedFSOptions -> Yaml.Value
example namespace imageName (SeaweedFSOptions {..}) = let Right x = Yaml.decodeEither' raw in x
 where
  -- Standalone S3 gateway (preferred over the deprecated filer-embedded S3). We turn off the
  -- embedded IAM (which, with no identities configured, denies everything) and instead point
  -- the gateway at a config secret holding a single admin identity (see 's3ConfigSecretYaml').
  s3Block :: Text
  s3Block = case seaweedFsS3 of
    Nothing -> ""
    Just _ -> [i|
  s3:
    replicas: 1
    iam: false
    configSecret:
      name: #{seaweedFsBaseName}-s3-config
      key: config.json|]
  -- The filer.toml store config, indented under @config: |@ (6 spaces, matching the post-dedent
  -- nesting). Defaults to the operator's embedded leveldb2.
  filerConfigBlock :: Text
  filerConfigBlock =
    let toml = fromMaybe "[leveldb2]\nenabled = true\ndir = \"/data/filerldb2\"" seaweedFsFilerConfig
    in mconcat ["\n      " <> l | l <- T.lines toml]
  -- See 'seaweedFsMasterConcurrentStart'.
  concurrentStartBlock :: Text
  concurrentStartBlock = if seaweedFsMasterConcurrentStart then "\n    concurrentStart: true" else ""
  -- Per-component @extraArgs:@ list (4-space indent), applied to master/volume/filer.
  extraArgsBlock :: Text
  extraArgsBlock
    | null seaweedFsServerExtraArgs = ""
    | otherwise = "\n    extraArgs:" <> mconcat ["\n    - \"" <> a <> "\"" | a <- seaweedFsServerExtraArgs]
  -- Per-component probe block, emitting only the timing fields the override sets.
  probeBlock :: Text -> Maybe SeaweedFSProbeOverride -> Text
  probeBlock _ Nothing = ""
  probeBlock key (Just p) =
    let field name sel = (\v -> "\n      " <> name <> ": " <> show v) <$> sel p
        fields = catMaybes [
            field "initialDelaySeconds" seaweedFsProbeInitialDelaySeconds
          , field "timeoutSeconds" seaweedFsProbeTimeoutSeconds
          , field "periodSeconds" seaweedFsProbePeriodSeconds
          , field "successThreshold" seaweedFsProbeSuccessThreshold
          , field "failureThreshold" seaweedFsProbeFailureThreshold
          ]
    in if null fields then "" else "\n    " <> key <> ":" <> mconcat fields
  raw = [__i|apiVersion: seaweed.seaweedfs.com/v1
             kind: Seaweed
             metadata:
               namespace: #{namespace}
               name: #{seaweedFsBaseName}
             spec:
               image: #{imageName}
               volumeServerDiskCount: #{seaweedFsVolumeServerDiskCount}
               hostSuffix: seaweed.abcdefg.com
               master:
                 replicas: #{seaweedFsMasterReplicas}#{concurrentStartBlock}#{extraArgsBlock}#{probeBlock "readinessProbe" seaweedFsMasterReadinessProbe}#{probeBlock "livenessProbe" seaweedFsMasterLivenessProbe}
                 volumeSizeLimitMB: #{seaweedFsVolumeSizeLimitMb}
               volume:
                 replicas: #{seaweedFsVolumeReplicas}#{extraArgsBlock}#{probeBlock "readinessProbe" seaweedFsVolumeReadinessProbe}#{probeBlock "livenessProbe" seaweedFsVolumeLivenessProbe}
                 requests:
                   storage: #{seaweedFsVolumeStorageRequest}
               filer:
                 replicas: #{seaweedFsFilerReplicas}#{extraArgsBlock}#{probeBlock "readinessProbe" seaweedFsFilerReadinessProbe}#{probeBlock "livenessProbe" seaweedFsFilerLivenessProbe}
                 config: |#{filerConfigBlock}#{s3Block}
             |]

-- | A @Secret@ holding the SeaweedFS S3 identities config (the @-config@ file for @weed s3@),
-- with a single admin identity using the gateway's configured credentials. Referenced by the
-- standalone S3 gateway (see the @s3@ block in 'example').
s3ConfigSecretYaml :: Text -> Text -> SeaweedFSS3Options -> Text
s3ConfigSecretYaml namespace baseName (SeaweedFSS3Options {..}) = [i|apiVersion: v1
kind: Secret
metadata:
  name: #{baseName}-s3-config
  namespace: #{namespace}
type: Opaque
stringData:
  config.json: |
    {"identities":[{"name":"#{seaweedFsS3AccessKey}","credentials":[{"accessKey":"#{seaweedFsS3AccessKey}","secretKey":"#{seaweedFsS3SecretKey}"}],"actions":["Admin"]}]}
|]

-- | Extract the filer JWT signing keys (write, read) from a security.toml: the @key@ under
-- @[jwt.filer_signing]@ and @[jwt.filer_signing.read]@ respectively. Either is 'Nothing' if its
-- table/key is absent or the document doesn't parse.
parseFilerJwtKeys :: Text -> (Maybe Text, Maybe Text)
parseFilerJwtKeys tomlText = case Toml.parse tomlText of
  Left _ -> (Nothing, Nothing)
  Right table ->
    ( lookupString table ["jwt", "filer_signing", "key"]
    , lookupString table ["jwt", "filer_signing", "read", "key"]
    )
  where
    -- Follow a dotted path of TOML tables to a final string value.
    lookupString :: Table' a -> [Text] -> Maybe Text
    lookupString _ [] = Nothing
    lookupString (MkTable m) [k] = case snd <$> Map.lookup k m of
      Just (Text' _ s) -> Just s
      _ -> Nothing
    lookupString (MkTable m) (k : ks) = case snd <$> Map.lookup k m of
      Just (Table' _ sub) -> lookupString sub ks
      _ -> Nothing

-- | Install the seaweedfs-csi-driver pointed at this filer, so PVCs on the @seaweedfs-storage@
-- 'StorageClass' provision against it. The @--filer@ flag takes the HTTP address, so we point it
-- at @\<baseName\>-filer.\<namespace\>:8888@. Waits for the driver to roll out.
installSeaweedFsCsiDriver :: (
  MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m, MonadFail m
  )
  -- | Path to the @nix@ binary (used to fetch the CSI manifest)
  => FilePath
  -- | Pinned nixpkgs (the test's nix context) to fetch the manifest with
  -> NixpkgsDerivation
  -- | Path to the @kubectl@ binary
  -> FilePath
  -- | Environment to run @kubectl@ with
  -> [(String, String)]
  -- | Namespace the SeaweedFS filer lives in
  -> Text
  -- | SeaweedFS base name (the filer Service is @\<baseName\>-filer@)
  -> Text
  -> SeaweedFSCsiDriverOptions
  -> m ()
installSeaweedFsCsiDriver nixBinary nixpkgsDerivation kubectlBinary env namespace baseName (SeaweedFSCsiDriverOptions {..}) = do
  let manifestUrl = [i|https://raw.githubusercontent.com/seaweedfs/seaweedfs-csi-driver/#{seaweedFsCsiDriverVersion}/deploy/kubernetes/seaweedfs-csi.yaml|] :: Text
  let filerAddress = [i|#{baseName}-filer.#{namespace}:8888|] :: Text

  timeAction "Apply CSI manifest" $ do
    info [i|Applying seaweedfs-csi-driver #{seaweedFsCsiDriverVersion} manifest, pointing it at filer '#{filerAddress}'|]
    -- Bake the filer address into the manifest's SEAWEEDFS_FILER placeholder up front, so it's a
    -- single rollout. Patching it after @apply@ instead double-rolls the controller, whose
    -- single-replica anti-affinity then deadlocks the new pod on a single-node cluster.
    manifestPath <- nixBuildExprToPath nixBinary (toString (renderDerivationWithPkgs nixpkgsDerivation (nixFetchUrlExpr manifestUrl)))
    manifest <- decodeUtf8 <$> readFileBS (toString manifestPath)
    let substituted = T.replace "SEAWEEDFS_FILER:8888" filerAddress manifest
    createProcessWithFileLoggingAndStdin' "seaweedfs-csi-apply"
      ((proc kubectlBinary ["apply", "-n", seaweedFsCsiNamespace, "-f", "-"]) { env = Just env }) (toString substituted)
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- Wait for the CSI controller and node plugin. We skip the seaweedfs-mount DaemonSet: it uses
  -- updateStrategy OnDelete, which @kubectl rollout status@ rejects; its pod is created anyway and
  -- the PVC mount exercises it.
  timeAction "Wait for CSI driver (controller/node)" $ do
    info [i|Waiting for the seaweedfs-csi-driver to be ready|]
    forM_ ([("deployment", "seaweedfs-controller"), ("daemonset", "seaweedfs-node")] :: [(String, String)]) $ \(kind, name) ->
      retryKubectl kubectlBinary env (toString ([i|seaweedfs-wait-#{name}|] :: Text))
        ["rollout", "status", "-n", seaweedFsCsiNamespace, [i|#{kind}/#{name}|], "--timeout=300s"]

-- | Run a @kubectl@ command, retrying on failure. Used for @rollout status@ on resources
-- the SeaweedFS operator creates asynchronously, which may not exist the instant we ask.
retryKubectl :: (
  MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m
  ) => FilePath -> [(String, String)] -> String -> [String] -> m ()
retryKubectl kubectlBinary env name args = go (2400 :: Int)
  where
    go n = handleAny (handler n) $
      createProcessWithFileLogging' name ((proc kubectlBinary args) { env = Just env })
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)
    handler n e
      | n <= 1 = throwIO e
      | otherwise = do
          debug [i|(#{name}) not ready yet (#{n - 1} attempts left): #{e}|]
          threadDelay 5_000_000
          go (n - 1)

-- | Run @nix build --expr@ on a Nix expression and return the resulting store path.
nixBuildExprToPath :: (
  MonadUnliftIO m, HasBaseContextMonad context m
  ) => FilePath -> String -> m Text
nixBuildExprToPath nixBinary expr = do
  let cp = proc nixBinary [
        "build", "--impure"
        , "--extra-experimental-features", "nix-command"
        , "--expr", expr
        , "--no-link"
        , "--json"
        ]
  out <- withFile "/dev/null" WriteMode $ \hNull ->
    readCreateProcess (cp { std_err = UseHandle hNull }) ""
  case A.eitherDecodeStrict (encodeUtf8 out) of
    Right (A.Array (V.toList -> ((A.Object (aesonLookup "outputs" -> Just (A.Object (aesonLookup "out" -> Just (A.String p))))):_))) -> pure p
    x -> expectationFailure [i|Couldn't parse nix build output: #{x}|]

-- | Fetch a URL to a store path so we can tweak the contents before applying. Expects @pkgs@ in
-- scope (wrapped by 'renderDerivationWithPkgs'). Impure -- fine under @--impure@.
nixFetchUrlExpr :: Text -> Text
nixFetchUrlExpr url = [i|pkgs.runCommand "fetched" {} "cp ${builtins.fetchurl "#{url}"} $out"|]

-- | The pinned seaweedfs-operator source, as a @pkgs.fetchFromGitHub@ expression (expects @pkgs@
-- in scope -- see 'renderDerivationWithPkgs'). Points at our @codedownio@ fork, which adds the
-- per-component probe-timing CR fields the @\*ReadinessProbe@\/@\*LivenessProbe@ options use, on
-- top of an upstream release. The CRD schema matches what 'example' produces. When bumping the
-- rev/sha256, also bump 'seaweedFsOperatorVendorHash'.
seaweedFsOperatorSrc :: Text
seaweedFsOperatorSrc = [__i|pkgs.fetchFromGitHub {
                              owner = "codedownio";
                              repo = "seaweedfs-operator";
                              rev = "79173edd2b07f9a6eb7eab842eb028f429bd0fc5";
                              sha256 = "sha256-8lYc2vzGpwr7q0UJ5JdyiPifrKR95pHZGFXMLq/QJik=";
                            }|]

-- | @go.sum@ vendor hash for the operator at 'seaweedFsOperatorSrc'. Bump together with the rev.
seaweedFsOperatorVendorHash :: Text
seaweedFsOperatorVendorHash = "sha256-NGL69t6vOL7ayrYOoStkU+fWm2zMv5MW6Fc1lQVeeMs="

-- | Builds the operator manager image with Nix (mirrors the upstream Dockerfile: a static
-- @manager@ binary at @/manager@, run as nonroot), output as a tarball loaded via
-- 'ImageLoadSpecTarball'. Expects @pkgs@ in scope. The manager is built with a dedicated pinned
-- nixpkgs because the operator needs Go 1.26.
defaultSeaweedFsOperatorImageExpr :: Text
defaultSeaweedFsOperatorImageExpr = [__i|
  let
    goPkgs = import (pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "3e41b24abd260e8f71dbe2f5737d24122f972158";
      sha256 = "sha256-rxO+uc/KFbSJp+pgyXRuAX6QlG9hJdnt0BXpEQRXY+U=";
    }) { system = pkgs.system; };
    manager = goPkgs.buildGo126Module {
      pname = "seaweedfs-operator-manager";
      version = "1.0.28";
      src = #{seaweedFsOperatorSrc};
      vendorHash = "#{seaweedFsOperatorVendorHash}";
      subPackages = [ "cmd" ];
      env.CGO_ENABLED = 0;
      postInstall = "mv $out/bin/cmd $out/bin/manager";
    };
  in
  pkgs.dockerTools.buildImage {
    name = "seaweedfs/seaweedfs-operator";
    tag = "nix";
    copyToRoot = pkgs.runCommand "seaweedfs-operator-manager-root" {} "mkdir -p $out; cp ${manager}/bin/manager $out/manager";
    config = {
      Entrypoint = [ "/manager" ];
      User = "65532:65532";
    };
  }
  |]

-- | Renders the operator's CRD and controller manifests with kustomize at build time, so at test
-- time we just @kubectl apply@ them. Expects @pkgs@ in scope. Bakes in the @:nix@ operator image,
-- strips invalid OpenAPI int32/int64 format fields from the CRDs, and outputs @crd.yaml@ +
-- @operator.yaml@.
seaweedFsOperatorManifestsExpr :: Text
seaweedFsOperatorManifestsExpr = [__i|
  pkgs.runCommand "seaweedfs-operator-manifests" { nativeBuildInputs = [ pkgs.kustomize pkgs.perl ]; } ''
    cp -r ${#{seaweedFsOperatorSrc}} src && chmod -R u+w src && cd src
    perl -i -pe 's/\\s+format: int32\\n//g; s/\\s+format: int64\\n//g' config/crd/bases/*.yaml
    (cd config/manager && kustomize edit set image controller:latest=seaweedfs/seaweedfs-operator:nix)
    mkdir -p $out
    kustomize build config/crd > $out/crd.yaml
    kustomize build config/default > $out/operator.yaml
  ''
|]
