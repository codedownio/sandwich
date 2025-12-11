{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers.HelmChart (
  withKataContainersHelmChart
  , withKataContainersHelmChart'
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


withKataContainersHelmChart :: (
  MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  , MonadReader context m, HasFile context "helm"
  )
  => KubernetesClusterContext
  -> KataContainersOptions
  -> String
  -> [String]
  -> (KataContainersContext -> m b)
  -> m b
withKataContainersHelmChart kcc options helmChart helmArgs action = do
  helmBinary <- askFile @"helm"
  withKataContainersHelmChart' helmBinary kcc options helmChart helmArgs action

withKataContainersHelmChart' :: (
  MonadFail m, MonadLoggerIO m, MonadUnliftIO m
  )
  => FilePath
  -> KubernetesClusterContext
  -> KataContainersOptions
  -> String
  -> [String]
  -> (KataContainersContext -> m b)
  -> m b
withKataContainersHelmChart' helmBinary kcc options helmChart helmArgs action = do
  let args = [
        "install", "kata-deploy"
        , helmChart
        , "--namespace", "kube-system"
        , "--wait"
        , "--timeout", "10m", "--atomic"
        -- , "--version", helmChartVersion
        ] <> helmArgs

  info [i|helm #{T.intercalate " " (fmap toText args)}|]

  env <- getKubectlEnvironment kcc

  createProcessWithLogging ((proc helmBinary args) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  action (KataContainersContext options)
