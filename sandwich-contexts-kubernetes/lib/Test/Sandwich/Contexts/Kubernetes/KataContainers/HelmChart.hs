{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers.HelmChart (
  withKataContainers
  , withKataContainers'
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import Relude hiding (withFile)
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.KataContainers.Types
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.Types
import UnliftIO.Process


withKataContainers :: (
  MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  , MonadReader context m, HasFile context "helm"
  )
  => KubernetesClusterContext
  -> KataContainersOptions
  -> (KataContainersContext -> m b)
  -> m b
withKataContainers kcc options action = do
  helmBinary <- askFile @"helm"
  withKataContainers' helmBinary kcc options action

withKataContainers' :: (
  MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  )
  => FilePath
  -> KubernetesClusterContext
  -> KataContainersOptions
  -> (KataContainersContext -> m b)
  -> m b
withKataContainers' helmBinary kcc options@(KataContainersOptions {..}) action = do
  let args = [
        "install", "kata-deploy"
        , kataContainersHelmChart
        , "--namespace", "kube-system"
        , "--wait"
        , "--timeout", "10m", "--atomic"
        -- , "--version", helmChartVersion
        ] <> kataContainersHelmArgs

  info [i|helm #{T.intercalate " " (fmap toText args)}|]

  env <- getKubectlEnvironment kcc

  createProcessWithLogging ((proc helmBinary args) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  action (KataContainersContext options)
