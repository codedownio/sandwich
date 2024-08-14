{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers (
  introduceKataContainers
  , withKataContainers
  , withKataContainers'

  , KataContainersOptions(..)
  , defaultKataContainersOptions

  , kataContainers
  , KataContainersContext(..)
  , HasKataContainersContext
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import Relude hiding (withFile)
import Safe
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
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
  } deriving (Show)
defaultKataContainersOptions :: KataContainersOptions
defaultKataContainersOptions = KataContainersOptions {
  kataContainersSourceCheckout = SourceCheckoutNixDerivation kataContainersDerivation
  }

kataContainers :: Label "kataContainers" KataContainersContext
kataContainers = Label
type HasKataContainersContext context = HasLabel context "kataContainers" KataContainersContext

introduceKataContainers :: (
  MonadUnliftIO m, MonadMask m, Typeable context, HasBaseContext context, HasKubernetesClusterContext context, HasNixContext context
  ) => KataContainersOptions -> SpecFree (LabelValue "kataContainers" KataContainersContext :> LabelValue "file-kubectl" (EnvironmentFile "kubectl") :> context) m () -> SpecFree context m ()
introduceKataContainers options = introduceBinaryViaNixPackage @"kubectl" "kubectl" . introduceWith "introduce KataContainers" kataContainers (void . withKataContainers options)

withKataContainers :: forall context m a. (
  HasCallStack, MonadMask m, MonadLoggerIO m, MonadUnliftIO m, Typeable context
  , HasBaseContextMonad context m, HasKubernetesClusterContext context, HasFile context "kubectl"
  ) => KataContainersOptions -> (KataContainersContext -> m a) -> m a
withKataContainers options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withKataContainers' kcc kubectlBinary options action

withKataContainers' :: forall context m a. (
  HasCallStack, MonadMask m, MonadLoggerIO m, MonadUnliftIO m, Typeable context
  , HasBaseContextMonad context m
  ) => KubernetesClusterContext -> FilePath -> KataContainersOptions -> (KataContainersContext -> m a) -> m a
withKataContainers' kcc@(KubernetesClusterContext {..}) kubectlBinary options@(KataContainersOptions {..}) action = do
  -- Preflight checks
  case kubernetesClusterType of
    KubernetesClusterKind {} -> expectationFailure [i|Can't install Kata Containers on Kind at present.|]
    KubernetesClusterMinikube {..} -> do
      output <- readCreateProcessWithLogging (proc minikubeBinary ["--profile", toString minikubeProfileName
                                                                  , "ssh", [i|egrep -c 'vmx|svm' /proc/cpuinfo|]]) ""
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

  (_, env) <- runWithKubectl' kcc kubectlBinary

  -- Now follow the instructions from
  -- https://github.com/kata-containers/kata-containers/blob/main/docs/install/minikube-installation-guide.md#installing-kata-containers

  -- Install kata-deploy
  createProcessWithLogging ((proc kubectlBinary ["apply", "-f", kataRoot </> "tools/packaging/kata-deploy/kata-rbac/base/kata-rbac.yaml"]) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
  createProcessWithLogging ((proc kubectlBinary ["apply", "-f", kataRoot </> "tools/packaging/kata-deploy/kata-deploy/base/kata-deploy.yaml"]) { env = Just env })
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

  action $ KataContainersContext options

kataContainersDerivation = [__i|{fetchFromGitHub}:

                                fetchFromGitHub {
                                  owner = "kata-containers";
                                  repo = "kata-containers";
                                  rev = "44b08b84b00d453b6e8a96d362c3f191c4b8257e";
                                  sha256 = "sha256-c8C3UdQ4PZF4gxN1ZskN30OJ64IY2/N5eAkUhtUHM7E=";
                                }
                               |]
