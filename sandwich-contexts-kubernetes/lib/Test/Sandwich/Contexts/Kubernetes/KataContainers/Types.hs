{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers.Types where

import Relude hiding (withFile)
import Test.Sandwich


data KataContainersContext = KataContainersContext {
  kataContainersOptions :: KataContainersOptions
  } deriving (Show)

data KataContainersOptions =
  KataContainersOptions {
    -- | Path or URL to a Helm chart
    kataContainersHelmChart :: FilePath
    -- | Extra arguments to pass to Helm
    , kataContainersHelmArgs :: [String]
    }
  deriving (Show)

defaultKataContainersOptions :: KataContainersOptions
defaultKataContainersOptions = KataContainersOptions {
  kataContainersHelmChart = "oci://ghcr.io/kata-containers/kata-deploy-charts/kata-deploy:3.23.0"
  , kataContainersHelmArgs = []
  }

kataContainers :: Label "kataContainers" KataContainersContext
kataContainers = Label
type HasKataContainersContext context = HasLabel context "kataContainers" KataContainersContext

type ContextWithKataContainers context =
  LabelValue "kataContainers" KataContainersContext
  :> context
