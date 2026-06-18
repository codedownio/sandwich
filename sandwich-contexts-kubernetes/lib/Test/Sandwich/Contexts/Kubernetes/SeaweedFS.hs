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

  -- * Types
  , SeaweedFSOptions(..)
  , defaultSeaweedFSOptions
  , defaultSeaweedFsOperatorImageExpr
  , SeaweedFSCsiDriverOptions(..)
  , defaultSeaweedFSCsiDriverOptions
  , SeaweedFSS3Options(..)
  , defaultSeaweedFSS3Options

  , seaweedFs
  , SeaweedFSContext(..)
  , HasSeaweedFSContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Data.Aeson as A
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Yaml as Yaml
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Images (loadImage')
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import Test.Sandwich.Contexts.Nix
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Environment
import UnliftIO.Exception (handleAny, throwIO)
import UnliftIO.IO (withFile)
import UnliftIO.Process


data SeaweedFSContext = SeaweedFSContext {
  seaweedFsOptions :: SeaweedFSOptions
  -- | The name of the @StorageClass@ provisioning SeaweedFS-backed volumes, if the
  -- CSI driver was installed (see 'seaweedFsCsiDriver')
  , seaweedFsCsiStorageClass :: Maybe Text
  -- | The operator-generated filer JWT signing keys (security.toml @jwt.filer_signing.key@ /
  -- @jwt.filer_signing.read.key@), read from the @\<baseName\>-security-config@ ConfigMap. The
  -- operator renders these unconditionally, so a client talking to the filer HTTP API directly
  -- must present a JWT signed with these. 'Nothing' if no security ConfigMap was found.
  , seaweedFsFilerJwtWriteKey :: Maybe Text
  , seaweedFsFilerJwtReadKey :: Maybe Text
  } deriving (Show)

data SeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage :: ImageLoadSpec
  -- | Extra images to preload into the cluster (so operator-managed pods don't pull them at
  -- runtime). Defaults to the operator's @kube-rbac-proxy@ sidecar.
  , seaweedFsExtraImages :: [ImageLoadSpec]
  -- | The Nix expression (built via the test's nix context) producing the operator manager image
  -- tarball. Defaults to 'defaultSeaweedFsOperatorImageExpr'.
  , seaweedFsOperatorImageExpr :: Text
  , seaweedFsBaseName :: Text
  , seaweedFsMasterReplicas :: Int
  , seaweedFsFilerReplicas :: Int
  , seaweedFsVolumeReplicas :: Int
  , seaweedFsVolumeServerDiskCount :: Int
  , seaweedFsVolumeSizeLimitMb :: Int
  , seaweedFsVolumeStorageRequest :: Text
  -- | If 'Just', deploy a standalone S3 gateway (@spec.s3@ in the Seaweed CR), exposing the
  -- S3 API on the @\<baseName\>-s3@ Service at port 8333, with a single admin identity using
  -- the given credentials. 'Nothing' disables the S3 gateway. Defaults to 'Just'
  -- 'defaultSeaweedFSS3Options'.
  , seaweedFsS3 :: Maybe SeaweedFSS3Options
  -- | Whether to install the
  -- [seaweedfs-csi-driver](https://github.com/seaweedfs/seaweedfs-csi-driver),
  -- which provisions Kubernetes @PersistentVolume@s backed by this SeaweedFS
  -- filer (via the @seaweedfs-storage@ 'StorageClass'). Defaults to 'Just'
  -- 'defaultSeaweedFSCsiDriverOptions'.
  , seaweedFsCsiDriver :: Maybe SeaweedFSCsiDriverOptions
  } deriving (Show)
defaultSeaweedFSOptions :: SeaweedFSOptions
defaultSeaweedFSOptions = SeaweedFSOptions {
  seaweedFsImage = ImageLoadSpecDocker "chrislusf/seaweedfs:4.33" IfNotPresent
  , seaweedFsExtraImages = [ImageLoadSpecDocker "registry.k8s.io/kubebuilder/kube-rbac-proxy:v0.16.0" IfNotPresent]
  , seaweedFsOperatorImageExpr = defaultSeaweedFsOperatorImageExpr
  , seaweedFsBaseName = "seaweed1"
  , seaweedFsMasterReplicas = 3
  , seaweedFsFilerReplicas = 2
  , seaweedFsVolumeReplicas = 1
  , seaweedFsVolumeServerDiskCount = 1
  , seaweedFsVolumeSizeLimitMb = 1024
  , seaweedFsVolumeStorageRequest = "2Gi"
  , seaweedFsS3 = Just defaultSeaweedFSS3Options
  , seaweedFsCsiDriver = Just defaultSeaweedFSCsiDriverOptions
  }

-- | Options for installing the SeaweedFS CSI driver.
data SeaweedFSCsiDriverOptions = SeaweedFSCsiDriverOptions {
  -- | The seaweedfs-csi-driver release (git tag) whose @deploy/kubernetes/seaweedfs-csi.yaml@
  -- manifest to install, e.g. @\"v1.4.5\"@.
  seaweedFsCsiDriverVersion :: Text
  -- | Images referenced by the CSI driver manifest, preloaded into the cluster before applying it
  -- (the manifest pins @imagePullPolicy: IfNotPresent@, so preloading avoids any runtime pull).
  -- Must match 'seaweedFsCsiDriverVersion'.
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

  -- The operator manager image, built entirely by Nix (see 'seaweedFsOperatorImageExpr').
  info [i|------------------ Building and loading SeaweedFS operator image ------------------|]
  operatorImageTarball <- timeAction "Build operator image (Nix)" $
    nixBuildExprToPath nixContextNixBinary (toString (renderWithPkgs (seaweedFsOperatorImageExpr options)))
  operatorImageName <- timeAction "Load operator image into cluster" $
    loadImage' kcc (ImageLoadSpecTarball (toString operatorImageTarball))
  info [i|Loaded operator image into cluster as: #{operatorImageName}|]

  info [i|------------------ Preloading SeaweedFS server image ------------------|]
  imageName <- timeAction "Preload SeaweedFS server image" $
    loadImage' kcc (seaweedFsImage options)

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
      installSeaweedFsCsiDriver nixContextNixBinary kubectlBinary env namespace (seaweedFsBaseName options) csiOptions
      pure (Just seaweedFsStorageClassName)

  -- Read the operator-generated security.toml so callers that hit the filer HTTP API directly
  -- can authenticate (the operator renders jwt.filer_signing(.read) keys unconditionally). The
  -- ConfigMap is created during reconcile and mounted into the (now-ready) filer, so it exists.
  (jwtWriteKey, jwtReadKey) <- do
    let cmName = seaweedFsBaseName options <> "-security-config"
    securityToml <- handleAny (\_ -> pure "") $ readCreateProcess
      ((proc kubectlBinary
        [ "get", "configmap", toString cmName, "-n", toString namespace
        , "-o", "jsonpath={.data.security\\.toml}" ]) { env = Just env }) ""
    let keys = parseFilerJwtKeys (T.pack securityToml)
    info [i|SeaweedFS filer JWT keys present: write=#{isJust (fst keys)} read=#{isJust (snd keys)}|]
    pure keys

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
  raw = [i|apiVersion: seaweed.seaweedfs.com/v1
kind: Seaweed
metadata:
  namespace: #{namespace}
  name: #{seaweedFsBaseName}
spec:
  image: #{imageName}
  volumeServerDiskCount: #{seaweedFsVolumeServerDiskCount}
  hostSuffix: seaweed.abcdefg.com
  master:
    replicas: #{seaweedFsMasterReplicas}
    volumeSizeLimitMB: #{seaweedFsVolumeSizeLimitMb}
  volume:
    replicas: #{seaweedFsVolumeReplicas}
    requests:
      storage: #{seaweedFsVolumeStorageRequest}
  filer:
    replicas: #{seaweedFsFilerReplicas}
    config: |
      [leveldb2]
      enabled = true
      dir = "/data/filerldb2"#{s3Block}
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

-- | Extract the filer JWT signing keys (write, read) from a security.toml — the @key = "..."@
-- under @[jwt.filer_signing]@ and @[jwt.filer_signing.read]@ respectively. Either is 'Nothing'
-- if its section/key is absent.
parseFilerJwtKeys :: Text -> (Maybe Text, Maybe Text)
parseFilerJwtKeys toml =
  let (_, w, r) = L.foldl' step (Nothing :: Maybe Text, Nothing, Nothing) (fmap T.strip (T.lines toml))
  in (w, r)
  where
    step (sec, w, r) line
      | "[" `T.isPrefixOf` line = (Just line, w, r)
      | "key" `T.isPrefixOf` line
      , Just v <- quoted line = case sec of
          Just "[jwt.filer_signing]" -> (sec, Just v, r)
          Just "[jwt.filer_signing.read]" -> (sec, w, Just v)
          _ -> (sec, w, r)
      | otherwise = (sec, w, r)
    quoted line = case T.splitOn "\"" line of
      (_ : v : _) -> Just v
      _ -> Nothing

-- | Install the seaweedfs-csi-driver and point it at this SeaweedFS filer, so that
-- @PersistentVolumeClaim@s using the @seaweedfs-storage@ 'StorageClass' get provisioned
-- against it. The driver's controller/node plugins connect to the filer over its gRPC
-- port (HTTP port + 10000), but the @--filer@ flag takes the HTTP address, so we point
-- it at @\<baseName\>-filer.\<namespace\>:8888@. Also waits for the SeaweedFS cluster
-- (master/volume/filer) and the driver to roll out, since provisioning needs the filer
-- reachable.
installSeaweedFsCsiDriver :: (
  MonadUnliftIO m, MonadLoggerIO m, HasBaseContextMonad context m, MonadFail m
  )
  -- | Path to the @nix@ binary (used to fetch the CSI manifest)
  => FilePath
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
installSeaweedFsCsiDriver nixBinary kubectlBinary env namespace baseName (SeaweedFSCsiDriverOptions {..}) = do
  let manifestUrl = [i|https://raw.githubusercontent.com/seaweedfs/seaweedfs-csi-driver/#{seaweedFsCsiDriverVersion}/deploy/kubernetes/seaweedfs-csi.yaml|] :: Text
  let filerAddress = [i|#{baseName}-filer.#{namespace}:8888|] :: Text

  timeAction "Apply CSI manifest" $ do
    info [i|Applying seaweedfs-csi-driver #{seaweedFsCsiDriverVersion} manifest, pointing it at filer '#{filerAddress}'|]
    -- Fetch the manifest (via Nix) and substitute the SEAWEEDFS_FILER placeholder before applying.
    -- The upstream manifest ships @value: "SEAWEEDFS_FILER:8888"@ as a placeholder on both the
    -- controller Deployment and node DaemonSet; baking in the real address up front means a single
    -- rollout. (Patching it after @apply@ with @kubectl set env@ instead double-rolls the controller,
    -- whose single-replica pod anti-affinity then deadlocks the new pod on a single-node cluster.)
    manifestPath <- nixBuildExprToPath nixBinary (nixFetchUrlExpr manifestUrl)
    manifest <- decodeUtf8 <$> readFileBS (toString manifestPath)
    let substituted = T.replace "SEAWEEDFS_FILER:8888" filerAddress manifest
    createProcessWithFileLoggingAndStdin' "seaweedfs-csi-apply"
      ((proc kubectlBinary ["apply", "-n", seaweedFsCsiNamespace, "-f", "-"]) { env = Just env }) (toString substituted)
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- Wait for the CSI controller (provisioner) and node plugin. We deliberately skip the
  -- seaweedfs-mount DaemonSet: it uses updateStrategy OnDelete, for which @kubectl rollout status@
  -- errors immediately ("only available for RollingUpdate"). Its pod is created on this fresh
  -- install regardless, and the actual PVC mount exercises it.
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

-- | A Nix expression that fetches a URL to a store path, so we can read and tweak the contents
-- before applying. Impure (no hash) -- fine under the @--impure@ 'nixBuildExprToPath' passes.
nixFetchUrlExpr :: Text -> String
nixFetchUrlExpr url = [i|with import <nixpkgs> {}; runCommand "fetched" {} "cp ${builtins.fetchurl "#{url}"} $out"|]

-- | The pinned seaweedfs-operator source, as a @pkgs.fetchFromGitHub@ expression (it expects
-- @pkgs@ in scope -- see 'renderDerivationWithPkgs'). This is 0.1.28, the latest release. Two
-- things to know about this pin:
--
--   * Its metrics auth-proxy sidecar uses the maintained
--     registry.k8s.io/kubebuilder/kube-rbac-proxy:v0.16.0. Older revs (e.g. 6fa4c24,
--     Jan 2025) pinned the since-removed gcr.io/kubebuilder/kube-rbac-proxy:v0.5.0, which
--     kept the controller-manager pod from starting.
--
--   * From PR #248 on, the operator unconditionally renders a security.toml with
--     @jwt.filer_signing(.read)@ keys even when TLS is off (to register the IAM gRPC
--     service). On older SeaweedFS images (e.g. 3.73) that makes the filer answer 401
--     ("wrong jwt") to the plain @GET /@ its own liveness/readiness probes hit, so the
--     filer CrashLoopBackOffs and never goes Ready (it has no JWT-exempt health route;
--     see seaweedfs#4891). Current SeaweedFS images (4.x, see 'seaweedFsImage') don't gate
--     @GET /@ that way -- which is why upstream CI, running @chrislusf/seaweedfs:latest@,
--     stays green. So this operator must be paired with a current 'seaweedFsImage'.
--
-- The Seaweed CRD schema matches what 'example' produces. When bumping the rev/sha256, also bump
-- 'seaweedFsOperatorVendorHash'.
seaweedFsOperatorSrc :: Text
seaweedFsOperatorSrc = [__i|pkgs.fetchFromGitHub {
                              owner = "seaweedfs";
                              repo = "seaweedfs-operator";
                              rev = "58705464c3eccb4894244bb7769e595ad8e23132";
                              sha256 = "sha256-QKd4s7AzHeKdMKxftNw5LobiGBq3de1MHjGDNgS65Y8=";
                            }|]

-- | @go.sum@ vendor hash for the operator at 'seaweedFsOperatorSrc'. Bump together with the rev.
seaweedFsOperatorVendorHash :: Text
seaweedFsOperatorVendorHash = "sha256-Au2zw9f4e6QMoxrYgieqqZryFpx2eD1uTMokAj70xqU="

-- | Builds the operator's manager image (replacing @make docker-build@, which would otherwise
-- shell out to @docker buildx@ at test time). Expects @pkgs@ in scope (wrapped by
-- 'renderDerivationWithPkgs'). Mirrors the upstream Dockerfile: a statically-linked @manager@
-- binary placed at @/manager@ (the path its Deployment's @command@ invokes), run as nonroot.
-- Needs Go 1.25, so the nix context's nixpkgs must be recent (>= 25.05). The result is a docker
-- image tarball, loaded into the cluster via 'ImageLoadSpecTarball'.
defaultSeaweedFsOperatorImageExpr :: Text
defaultSeaweedFsOperatorImageExpr = [__i|
  let
    manager = pkgs.buildGo125Module {
      pname = "seaweedfs-operator-manager";
      version = "0.1.28";
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
-- time we just @kubectl apply@ them -- no source checkout, temp dir, or @make@ (replacing
-- @make install@ / @make deploy@). Expects @pkgs@ in scope. Bakes in the @:nix@ image built by
-- 'seaweedFsOperatorImageExpr', and mirrors @make manifests@' fix of stripping invalid OpenAPI
-- int32/int64 format fields from the CRDs. Outputs a directory with @crd.yaml@ and @operator.yaml@.
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
