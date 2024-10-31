{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-|

Install [Kata Containers](https://katacontainers.io) on a Kubernetes cluster.

-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers (
  -- * Introduce Kata Containers
  introduceKataContainers

  -- * Bracket-style versions
  , withKataContainers
  , withKataContainers'

  -- * Types
  , KataContainersOptions(..)
  , defaultKataContainersOptions

  , kataContainers
  , KataContainersContext(..)
  , HasKataContainersContext
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Data.Aeson (FromJSON)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Yaml
import Kubernetes.OpenAPI.Model as Kubernetes
import Kubernetes.OpenAPI.ModelLens as Kubernetes
import Relude hiding (withFile)
import Safe
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.FindImages
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Waits
import UnliftIO.Process


data KataContainersContext = KataContainersContext {
  kataContainersOptions :: KataContainersOptions
  } deriving (Show)

data SourceCheckout =
  SourceCheckoutFilePath FilePath
  | SourceCheckoutNixDerivation Text
  deriving (Show, Eq)

data KataContainersOptions = KataContainersOptions {
  kataContainersSourceCheckout :: SourceCheckout
  -- | If set, this will overwrite the image in the DaemonSet in @kata-deploy.yaml@ and will set the 'ImagePullPolicy'
  -- to 'IfNotPresent'.
  -- This is useful because it's currently (8\/15\/2024) set to @quay.io\/kata-containers\/kata-deploy:latest@,
  -- with @imagePullPolicy: Always@. This is not reproducible and also doesn't allow us to cache images.
  , kataContainersKataDeployImage :: Maybe Text
  -- | Whether to pull the image using Docker and load it onto the cluster using 'loadImageIfNecessary''.
  , kataContainersPreloadImages :: Bool
  -- | Whether to label the node(s) with @katacontainers.io/kata-runtime=true@, since this seems not to happen
  -- automatically with kata-deploy.
  , kataContainersLabelNode :: Bool
  } deriving (Show)
defaultKataContainersOptions :: KataContainersOptions
defaultKataContainersOptions = KataContainersOptions {
  kataContainersSourceCheckout = SourceCheckoutNixDerivation kataContainersDerivation
  , kataContainersKataDeployImage = Just "quay.io/kata-containers/kata-deploy:3.9.0"
  , kataContainersPreloadImages = True
  , kataContainersLabelNode = True
  }

kataContainers :: Label "kataContainers" KataContainersContext
kataContainers = Label
type HasKataContainersContext context = HasLabel context "kataContainers" KataContainersContext

type ContextWithKataContainers context =
  LabelValue "kataContainers" KataContainersContext
  :> LabelValue "file-kubectl" (EnvironmentFile "kubectl")
  :> context

-- | Install Kata Containers on the cluster and introduce a 'KataContainersContext'.
introduceKataContainers :: (
  MonadMask m, Typeable context, KubernetesClusterBasicWithoutReader context m, HasNixContext context
  )
  -- | Options
  => KataContainersOptions
  -> SpecFree (ContextWithKataContainers context) m ()
  -> SpecFree context m ()
introduceKataContainers options = introduceBinaryViaNixPackage @"kubectl" "kubectl" . introduceWith "introduce KataContainers" kataContainers (void . withKataContainers options)

-- | Bracket-style version of 'introduceKataContainers'.
withKataContainers :: forall context m a. (
  HasCallStack, Typeable context, MonadFail m, MonadMask m, KubectlBasic context m
  )
  -- | Options
  => KataContainersOptions
  -> (KataContainersContext -> m a)
  -> m a
withKataContainers options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withKataContainers' kcc kubectlBinary options action

-- | Same as 'withKataContainers', but allows you to pass in the 'KubernetesClusterContext' and @kubectl@ binary path.
withKataContainers' :: forall context m a. (
  HasCallStack, Typeable context, MonadFail m, MonadMask m, KubernetesBasic context m
  )
  => KubernetesClusterContext
  -- | Path to @kubectl@ binary
  -> FilePath
  -> KataContainersOptions
  -> (KataContainersContext -> m a)
  -> m a
withKataContainers' kcc@(KubernetesClusterContext {..}) kubectlBinary options@(KataContainersOptions {..}) action = do
  -- Preflight checks
  case kubernetesClusterType of
    KubernetesClusterKind {} -> expectationFailure [i|Can't install Kata Containers on Kind at present.|]
    KubernetesClusterMinikube {..} -> do
      output <- readCreateProcessWithLogging (proc kubernetesClusterTypeMinikubeBinary [
                                                 "--profile", toString kubernetesClusterTypeMinikubeProfileName
                                                 , "ssh", [i|egrep -c 'vmx|svm' /proc/cpuinfo|]
                                                 ]) ""
      case readMay output of
        Just (0 :: Int) -> expectationFailure [i|Preflight check: didn't find "vmx" or "svm" in /proc/cpuinfo. Please make sure virtualization support is enabled.|]
        Just _ -> return ()
        Nothing -> expectationFailure [i|Preflight check: couldn't parse output of minikube ssh "egrep -c 'vmx|svm' /proc/cpuinfo"|]

  -- Get Kata source dir
  kataRoot <- case kataContainersSourceCheckout of
    SourceCheckoutFilePath x -> pure x
    SourceCheckoutNixDerivation d -> getContextMaybe nixContext >>= \case
      Nothing -> expectationFailure [i|Wanted to build Kata Containers source checkout via derivation, but no Nix context was provided.|]
      Just nc -> buildNixCallPackageDerivation' nc d

  info [i|kataRoot: #{kataRoot}|]

  env <- askKubectlEnvironment kcc

  -- Now follow the instructions from
  -- https://github.com/kata-containers/kata-containers/blob/main/docs/install/minikube-installation-guide.md#installing-kata-containers

  -- Read the RBAC and DaemonSet configs
  rbacContents <- liftIO $ T.readFile $ kataRoot </> "tools/packaging/kata-deploy/kata-rbac/base/kata-rbac.yaml"
  deploymentContents' <- liftIO $ T.readFile $ kataRoot </> "tools/packaging/kata-deploy/kata-deploy/base/kata-deploy.yaml"
  let deploymentContents = case kataContainersKataDeployImage of
        Nothing -> deploymentContents'
        Just deployImage -> deploymentContents'
                          & setDaemonSetImage deployImage

  -- Preload any images
  when kataContainersPreloadImages $ do
    let images = findAllImages (rbacContents <> "\n---\n" <> deploymentContents)

    forM_ images $ \image -> do
      info [i|Preloading image: #{image}|]
      loadImageIfNecessary' kcc (ImageLoadSpecDocker image IfNotPresent)

  -- Install kata-deploy
  createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString rbacContents)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
  createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString deploymentContents)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  podName <- waitUntil 600 $ do
    pods <- (T.words . toText) <$> readCreateProcessWithLogging ((
      (proc "kubectl" ["-n", "kube-system"
                      , "get", "pods", "-o=name"]) { env = Just env }
      ) { env = Just env }) ""

    case headMay [t | t <- pods, "pod/kata-deploy" `T.isPrefixOf` t] of
      Just x -> pure x
      Nothing -> expectationFailure [i|Couldn't find kata-deploy pod in: #{pods}|]

  info [i|Got podName: #{podName}|]

  -- Wait until the kata-deploy pod starts sleeping
  waitUntil 600 $ do
    (exitCode, sout, serr) <- readCreateProcessWithExitCode (
      (proc kubectlBinary ["-n", "kube-system"
                          , "exec", toString podName
                          , "--"
                          , "ps", "-ef"
                          ])
      { env = Just env }
      ) ""
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure n -> expectationFailure [i|Command failed with code #{n}. Stderr: #{serr}|]

    toText sout `textShouldContain` "sleep infinity"

  -- Now install the runtime classes
  runtimeClassesContents <- liftIO $ T.readFile $ kataRoot </> "tools/packaging/kata-deploy/runtimeclasses/kata-runtimeClasses.yaml"
  createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString runtimeClassesContents)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- Finally, label the node(s)
  when kataContainersLabelNode $ do
    createProcessWithLoggingAndStdin ((proc kubectlBinary ["label", "nodes", "--all", "--overwrite", "katacontainers.io/kata-runtime=true"]) { env = Just env }) (toString deploymentContents)
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  action $ KataContainersContext options

kataContainersDerivation = [__i|{fetchFromGitHub}:

                                fetchFromGitHub {
                                  owner = "kata-containers";
                                  repo = "kata-containers";
                                  rev = "cdaaf708a18da8e5f7e2b9824fa3e43b524893a5";
                                  sha256 = "sha256-aBcu59LybgZ9xkCDUzZXb60FeClQNG1ivfC6lWQdlb0=";
                                }
                               |]

setDaemonSetImage :: Text -> Text -> Text
setDaemonSetImage image = mconcat . fmap setDaemonSetImage' . T.splitOn "---\n"
  where
    setDaemonSetImage' :: Text -> Text
    setDaemonSetImage' (decode -> Right x@(V1DaemonSet {v1DaemonSetKind=(Just "DaemonSet")})) = x
      & set (v1DaemonSetSpecL . _Just . v1DaemonSetSpecTemplateL . v1PodTemplateSpecSpecL . _Just . v1PodSpecContainersL . ix 0 . v1ContainerImageL) (Just image)
      & set (v1DaemonSetSpecL . _Just . v1DaemonSetSpecTemplateL . v1PodTemplateSpecSpecL . _Just . v1PodSpecContainersL . ix 0 . v1ContainerImagePullPolicyL) (Just "IfNotPresent")
      & Yaml.encode
      & decodeUtf8
    setDaemonSetImage' t = t

decode :: FromJSON a => Text -> Either Yaml.ParseException a
decode = Yaml.decodeEither' . encodeUtf8
