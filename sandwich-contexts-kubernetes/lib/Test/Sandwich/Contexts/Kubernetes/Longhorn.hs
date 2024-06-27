{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Sandwich.Contexts.Kubernetes.Longhorn (
  introduceLonghorn
  , withLonghorn
  , withLonghorn'

  , LonghornOptions(..)
  , defaultLonghornOptions

  , longhorn
  , LonghornContext(..)
  , HasLonghornContext
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Relude hiding (withFile)
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Nix
import UnliftIO.Environment
import UnliftIO.Process


data LonghornContext = LonghornContext {
  longhornOptions :: LonghornOptions
  } deriving (Show)

data LonghornOptions = LonghornOptions {
  longhornYaml :: String
  } deriving (Show)
defaultLonghornOptions :: LonghornOptions
defaultLonghornOptions = LonghornOptions {
  longhornYaml = "https://raw.githubusercontent.com/longhorn/longhorn/v1.6.2/deploy/longhorn.yaml"
  }

longhorn :: Label "longhorn" LonghornContext
longhorn = Label
type HasLonghornContext context = HasLabel context "longhorn" LonghornContext

introduceLonghorn :: (
  HasBaseContext context, HasKubernetesClusterContext context, MonadUnliftIO m, HasNixContext context
  ) => LonghornOptions -> SpecFree (LabelValue "longhorn" LonghornContext :> LabelValue "file-kubectl" (EnvironmentFile "kubectl") :> context) m () -> SpecFree context m ()
introduceLonghorn options =
  introduceBinaryViaNixPackage @"kubectl" "kubectl"
  . introduceWith "introduce Longhorn" longhorn (void . withLonghorn options)

withLonghorn :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  , HasBaseContextMonad context m, HasKubernetesClusterContext context, HasFile context "kubectl"
  ) => LonghornOptions -> (LonghornContext -> m a) -> m a
withLonghorn options action = do
  kcc <- getContext kubernetesCluster
  kubectlBinary <- askFile @"kubectl"
  withLonghorn' kcc kubectlBinary options action

withLonghorn' :: forall m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  ) => KubernetesClusterContext -> String -> LonghornOptions -> (LonghornContext -> m a) -> m a
withLonghorn' (KubernetesClusterContext {kubernetesClusterKubeConfigPath}) kubectlBinary options@(LonghornOptions {..}) action = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  createProcessWithLogging ((proc kubectlBinary ["apply", "-f", longhornYaml]) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  action $ LonghornContext options
