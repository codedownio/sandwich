{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|

Install a single-pod [SeaweedFS](https://github.com/seaweedfs/seaweedfs) on a Kubernetes cluster,
without the operator: one @weed server@ process runs master + volume + filer (+ optional S3) in a
single pod. Comes up much faster than "Test.Sandwich.Contexts.Kubernetes.SeaweedFS" (no CRDs, no
operator image build, no controller-manager wait), at the cost of being single-node only. Produces
the same 'SeaweedFSContext', so consumers don't change.

-}

module Test.Sandwich.Contexts.Kubernetes.SeaweedFSMini (
  introduceSeaweedFSMini

  -- * Bracket-style variants
  , withSeaweedFSMini
  , withSeaweedFSMini'

  -- * Options
  , defaultSeaweedFSMiniOptions
  , SeaweedFSMiniOptions(..)

  -- * Re-exports
  , SeaweedFSContext(..)
  , HasSeaweedFSContext
  , seaweedFs
  ) where

import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Images (loadImage', loadImageIfNecessary')
import Test.Sandwich.Contexts.Kubernetes.SeaweedFS
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix
import UnliftIO.Environment
import UnliftIO.Process


-- | Options for the single-pod \"mini\" SeaweedFS (see 'introduceSeaweedFSMini'). Unlike
-- 'SeaweedFSOptions', there's no operator, no replicas, and no per-component probe tuning: one
-- @weed server@ process runs master + volume + filer (+ optional S3) in a single pod.
data SeaweedFSMiniOptions = SeaweedFSMiniOptions {
  seaweedFsMiniImage :: ImageLoadSpec
  -- | Name prefix for the created resources. The all-in-one @Deployment@ is @\<baseName\>-mini@,
  -- and the Services are @\<baseName\>-master@ \/ @\<baseName\>-filer@ \/ @\<baseName\>-s3@, matching
  -- the operator-based names so consumers are the same.
  , seaweedFsMiniBaseName :: Text
  -- | Extra args appended to @weed server@. Defaults to @[\"-ip.bind=0.0.0.0\"]@ (avoids the
  -- startup DNS bind crash-loop), matching 'fastSeaweedFSOptions'.
  , seaweedFsMiniExtraArgs :: [Text]
  , seaweedFsMiniVolumeSizeLimitMb :: Int
  -- | If 'Just', run the embedded S3 gateway (@weed server -s3@) with a single admin identity
  -- using these credentials. Defaults to 'Just' 'defaultSeaweedFSS3Options'.
  , seaweedFsMiniS3 :: Maybe SeaweedFSS3Options
  -- | Whether to install the seaweedfs-csi-driver pointed at this pod's filer, providing the
  -- @seaweedfs-storage@ 'StorageClass'. Defaults to 'Just' 'defaultSeaweedFSCsiDriverOptions'.
  , seaweedFsMiniCsiDriver :: Maybe SeaweedFSCsiDriverOptions
  } deriving (Show)

defaultSeaweedFSMiniOptions :: SeaweedFSMiniOptions
defaultSeaweedFSMiniOptions = SeaweedFSMiniOptions {
  seaweedFsMiniImage = ImageLoadSpecDocker "chrislusf/seaweedfs:4.33" IfNotPresent
  , seaweedFsMiniBaseName = "seaweed1"
  , seaweedFsMiniExtraArgs = ["-ip.bind=0.0.0.0"]
  , seaweedFsMiniVolumeSizeLimitMb = 1024
  , seaweedFsMiniS3 = Just defaultSeaweedFSS3Options
  , seaweedFsMiniCsiDriver = Just defaultSeaweedFSCsiDriverOptions
  }


-- | Introduce a single-pod [SeaweedFS](https://github.com/seaweedfs/seaweedfs) on the Kubernetes
-- cluster, in a given namespace, using @weed server@ (master + volume + filer + optional S3) in one
-- pod. Unlike 'introduceSeaweedFS' this skips the operator entirely -- no CRDs, no operator image
-- build, no controller-manager wait -- so it comes up much faster, at the cost of being single-node
-- only. Produces the same 'SeaweedFSContext' (including the CSI 'StorageClass'), so consumers don't
-- change.
introduceSeaweedFSMini :: (
  KubernetesClusterBasicWithoutReader context m, HasNixContext context
  )
  -- | Namespace
  => Text
  -> SeaweedFSMiniOptions
  -> SpecFree (ContextWithSeaweedFS context) m ()
  -> SpecFree context m ()
introduceSeaweedFSMini namespace options = introduceBinaryViaNixPackage @"kubectl" "kubectl" . introduceWith "introduce SeaweedFS (mini)" seaweedFs (void . withSeaweedFSMini namespace options)

-- | Bracket-style version of 'introduceSeaweedFSMini'.
withSeaweedFSMini :: forall context m a. (
  HasCallStack, MonadFail m, KubectlBasic context m, HasNixContext context
  )
  -- | Namespace
  => Text
  -> SeaweedFSMiniOptions
  -> (SeaweedFSContext -> m a)
  -> m a
withSeaweedFSMini namespace options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withSeaweedFSMini' kcc kubectlBinary namespace options action

-- | Same as 'withSeaweedFSMini', but allows you to pass in the 'KubernetesClusterContext' and
-- @kubectl@ binary path.
withSeaweedFSMini' :: forall context m a. (
  HasCallStack, MonadFail m, NixContextBasic context m
  )
  -- | Cluster context
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -- | Namespace
  -> Text
  -> SeaweedFSMiniOptions
  -> (SeaweedFSContext -> m a)
  -> m a
withSeaweedFSMini' kcc@(KubernetesClusterContext {kubernetesClusterKubeConfigPath}) kubectlBinary namespace options@(SeaweedFSMiniOptions {..}) action = do
  baseEnv <- getEnvironment

  NixContext {..} <- getContext nixContext

  let env = baseEnv
          & (("KUBECONFIG", kubernetesClusterKubeConfigPath) :)
          & L.nubBy (\x y -> fst x == fst y)

  storageClass <- timeAction "SeaweedFS mini setup" $ do
    info [i|------------------ Preloading SeaweedFS server image ------------------|]
    imageName <- timeAction "Preload SeaweedFS server image" $
      loadImageIfNecessary' kcc seaweedFsMiniImage

    -- The S3 gateway reads its identities from this secret; create it before the pod so the mount
    -- resolves. Done with @apply@ so a re-run against an existing namespace is fine.
    whenJust seaweedFsMiniS3 $ \s3Options -> timeAction "Create S3 config secret" $
      createProcessWithFileLoggingAndStdin' "seaweedfs-s3-secret" ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString (s3ConfigSecretYaml namespace seaweedFsMiniBaseName s3Options))
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    info [i|------------------ Creating SeaweedFS (mini) deployment ------------------|]
    timeAction "Apply SeaweedFS mini manifests" $
      createProcessWithFileLoggingAndStdin' "seaweedfs-mini-apply" ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString (miniManifestsYaml namespace imageName options))
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    timeAction "Wait for SeaweedFS mini deployment" $ do
      info [i|Waiting for the SeaweedFS (mini) deployment to be ready|]
      retryKubectl kubectlBinary env "seaweedfs-mini-wait"
        ["rollout", "status", "-n", toString namespace, toString ([i|deployment/#{seaweedFsMiniBaseName}-mini|] :: Text), "--timeout=300s"]

    case seaweedFsMiniCsiDriver of
      Nothing -> do
        info [i|Skipping SeaweedFS CSI driver installation|]
        pure Nothing
      Just csiOptions -> timeAction "Install CSI driver" $ do
        info [i|------------------ Preloading SeaweedFS CSI driver images ------------------|]
        forM_ (seaweedFsCsiDriverImages csiOptions) $ \spec -> do
          loaded <- loadImage' kcc spec
          info [i|Preloaded CSI image: #{loaded}|]
        info [i|------------------ Installing SeaweedFS CSI driver ------------------|]
        installSeaweedFsCsiDriver nixContextNixBinary nixContextNixpkgsDerivation kubectlBinary env namespace seaweedFsMiniBaseName csiOptions
        pure (Just seaweedFsStorageClassName)

  -- Map the mini options onto a 'SeaweedFSOptions' for the context's @seaweedFsOptions@ field, so a
  -- consumer reading the base name / S3 credentials / CSI setting off the context works the same as
  -- with the operator-based 'withSeaweedFS'. The mini pod runs no JWT security.toml, so both filer
  -- JWT keys are 'Nothing'.
  let equivalentOptions = defaultSeaweedFSOptions {
        seaweedFsImage = seaweedFsMiniImage
        , seaweedFsBaseName = seaweedFsMiniBaseName
        , seaweedFsMasterReplicas = 1
        , seaweedFsFilerReplicas = 1
        , seaweedFsVolumeReplicas = 1
        , seaweedFsServerExtraArgs = seaweedFsMiniExtraArgs
        , seaweedFsVolumeSizeLimitMb = seaweedFsMiniVolumeSizeLimitMb
        , seaweedFsS3 = seaweedFsMiniS3
        , seaweedFsCsiDriver = seaweedFsMiniCsiDriver
        }

  action $ SeaweedFSContext {
    seaweedFsOptions = equivalentOptions
    , seaweedFsCsiStorageClass = storageClass
    , seaweedFsFilerJwtWriteKey = Nothing
    , seaweedFsFilerJwtReadKey = Nothing
    }


-- | Manifests for the single-pod SeaweedFS: an all-in-one @Deployment@ running @weed server@, plus
-- @master@\/@filer@\/@s3@ Services selecting it (named to match the operator-based deployment). Data
-- lives on an @emptyDir@ -- fine for ephemeral tests, and it means no PVC provisioning to wait on.
miniManifestsYaml :: Text -> Text -> SeaweedFSMiniOptions -> Text
miniManifestsYaml namespace imageName (SeaweedFSMiniOptions {..}) =
  T.intercalate "\n---\n" (deployment : services)
 where
  appLabel = seaweedFsMiniBaseName <> "-mini"
  s3Enabled = isJust seaweedFsMiniS3

  -- @weed server@ runs master + volume by default; @-filer@ adds the filer, @-s3@ the S3 gateway.
  args :: [Text]
  args = ["server", "-dir=/data", [i|-master.volumeSizeLimitMB=#{seaweedFsMiniVolumeSizeLimitMb}|], "-filer"]
         <> (if s3Enabled then ["-s3", "-s3.config=/etc/seaweedfs/s3/config.json"] else [])
         <> seaweedFsMiniExtraArgs
  argsYaml = "[" <> T.intercalate ", " ["\"" <> a <> "\"" | a <- args] <> "]"

  s3PortLine, s3VolumeMount, s3Volume :: Text
  s3PortLine = if s3Enabled then "\n        - { containerPort: 8333 }" else ""
  s3VolumeMount = if s3Enabled then "\n        - name: s3-config\n          mountPath: /etc/seaweedfs/s3\n          readOnly: true" else ""
  s3Volume = if s3Enabled then "\n      - name: s3-config\n        secret:\n          secretName: " <> seaweedFsMiniBaseName <> "-s3-config" else ""

  deployment = [__i|apiVersion: apps/v1
                    kind: Deployment
                    metadata:
                      name: #{appLabel}
                      namespace: #{namespace}
                      labels:
                        app: #{appLabel}
                    spec:
                      replicas: 1
                      selector:
                        matchLabels:
                          app: #{appLabel}
                      template:
                        metadata:
                          labels:
                            app: #{appLabel}
                        spec:
                          containers:
                          - name: seaweedfs
                            image: #{imageName}
                            command: ["weed"]
                            args: #{argsYaml}
                            ports:
                            - { containerPort: 9333 }
                            - { containerPort: 19333 }
                            - { containerPort: 8080 }
                            - { containerPort: 18080 }
                            - { containerPort: 8888 }
                            - { containerPort: 18888 }#{s3PortLine}
                            readinessProbe:
                              httpGet:
                                path: /
                                port: 8888
                              initialDelaySeconds: 2
                              periodSeconds: 2
                            volumeMounts:
                            - name: data
                              mountPath: /data#{s3VolumeMount}
                          volumes:
                          - name: data
                            emptyDir: {}#{s3Volume}|]

  -- @<baseName>-filer@ exposes both the filer HTTP (8888) and gRPC (18888) ports; the CSI driver
  -- (see 'installSeaweedFsCsiDriver') resolves this Service by that exact name.
  services = [
      svc (seaweedFsMiniBaseName <> "-master") (portItem "master-http" 9333 <> portItem "master-grpc" 19333)
    , svc (seaweedFsMiniBaseName <> "-filer") (portItem "filer-http" 8888 <> portItem "filer-grpc" 18888)
    ] <> [svc (seaweedFsMiniBaseName <> "-s3") (portItem "s3" 8333) | s3Enabled]

  portItem :: Text -> Int -> Text
  portItem name port = "\n  - { name: " <> name <> ", port: " <> show port <> ", targetPort: " <> show port <> " }"

  svc :: Text -> Text -> Text
  svc name ports = [__i|apiVersion: v1
                        kind: Service
                        metadata:
                          name: #{name}
                          namespace: #{namespace}
                        spec:
                          selector:
                            app: #{appLabel}
                          ports:#{ports}|]
