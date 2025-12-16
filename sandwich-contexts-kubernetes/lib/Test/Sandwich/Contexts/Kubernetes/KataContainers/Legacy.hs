{-# LANGUAGE DataKinds #-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers.Legacy (
  withKataContainersLegacy
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
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
import Test.Sandwich.Contexts.Kubernetes.FindImages
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.KataContainers.Types
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Waits
import UnliftIO.Process


withKataContainersLegacy :: (
  MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  , MonadReader context m, Typeable context, HasBaseContext context
  )
  => KubernetesClusterContext
  -> FilePath
  -> KataContainersOptions
  -> SourceCheckout
  -> Maybe Text
  -> Bool
  -> Bool
  -> (KataContainersContext -> m b)
  -> m b
withKataContainersLegacy kcc kubectlBinary options sourceCheckout deployImage preloadImages labelNode action = do
  -- Get Kata source dir
  kataRoot <- case sourceCheckout of
    SourceCheckoutFilePath x -> pure x
    SourceCheckoutNixDerivation d -> getContextMaybe nixContext >>= \case
      Nothing -> expectationFailure [i|Wanted to build Kata Containers source checkout via derivation, but no Nix context was provided.|]
      Just nc -> buildNixCallPackageDerivation' nc d

  info [i|kataRoot: #{kataRoot}|]

  env <- getKubectlEnvironment kcc

  -- Now follow the instructions from
  -- https://github.com/kata-containers/kata-containers/blob/main/docs/install/minikube-installation-guide.md#installing-kata-containers

  -- Read the RBAC and DaemonSet configs
  rbacContents <- liftIO $ T.readFile $ kataRoot </> "tools/packaging/kata-deploy/kata-rbac/base/kata-rbac.yaml"
  deploymentContents' <- liftIO $ T.readFile $ kataRoot </> "tools/packaging/kata-deploy/kata-deploy/base/kata-deploy.yaml"
  let deploymentContents = case deployImage of
        Nothing -> deploymentContents'
        Just di -> deploymentContents'
                   & setDaemonSetImage di

  -- Preload any images
  when preloadImages $ do
    let images = findAllImages (rbacContents <> "\n---\n" <> deploymentContents)

    forM_ images $ \image -> do
      info [i|Preloading image: #{image}|]
      loadImageIfNecessary' kcc (ImageLoadSpecDocker image IfNotPresent)

  -- Install kata-deploy
  debug [i|Applying kata-rbac.yaml|]
  createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString rbacContents)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
  debug [i|Applying kata-deploy.yaml|]
  createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString deploymentContents)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  debug [i|Waiting for kata-deploy pod to exist|]
  podName <- waitUntil 600 $ do
    pods <- (T.words . toText) <$> readCreateProcessWithLogging ((
      (proc kubectlBinary ["-n", "kube-system"
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
  debug [i|Applying kata-runtimeClasses.yaml|]
  runtimeClassesContents <- liftIO $ T.readFile $ kataRoot </> "tools/packaging/kata-deploy/runtimeclasses/kata-runtimeClasses.yaml"
  createProcessWithLoggingAndStdin ((proc kubectlBinary ["apply", "-f", "-"]) { env = Just env }) (toString runtimeClassesContents)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- Finally, label the node(s)
  debug [i|Labeling nodes with katacontainers.io/kata-runtime=true|]
  when labelNode $ do
    createProcessWithLoggingAndStdin ((proc kubectlBinary ["label", "nodes", "--all", "--overwrite", "katacontainers.io/kata-runtime=true"]) { env = Just env }) (toString deploymentContents)
      >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  action $ KataContainersContext options

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
