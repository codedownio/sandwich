{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KataContainers.Types where

import Data.String.Interpolate
import Relude hiding (withFile)
import Test.Sandwich


data KataContainersContext = KataContainersContext {
  kataContainersOptions :: KataContainersOptions
  } deriving (Show)

data SourceCheckout =
  SourceCheckoutFilePath FilePath
  | SourceCheckoutNixDerivation Text
  deriving (Show, Eq)

data KataContainersOptions =
  -- | Traditional install method, based on a source checkout
  KataContainersOptionsLegacy {
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
    }
  -- | Install Kata using a Helm chart
  | KataContainersOptionsHelmChart {
      -- | Path to the @helm@ binary
      kataContainersHelmBinary :: FilePath
      -- | Path or URL to a Helm chart
      , kataContainersHelmChart :: FilePath
      -- | Extra arguments to pass to Helm
      , kataContainersHelmArgs :: [String]
      }
  deriving (Show)

defaultKataContainersOptionsLegacy :: KataContainersOptions
defaultKataContainersOptionsLegacy = KataContainersOptionsLegacy {
  kataContainersSourceCheckout = SourceCheckoutNixDerivation kataContainersDerivation
  , kataContainersKataDeployImage = Just kataContainersDeployImage
  , kataContainersPreloadImages = True
  , kataContainersLabelNode = True
  }
  where
    kataContainersDeployImage :: Text
    kataContainersDeployImage = "quay.io/kata-containers/kata-deploy:3.23.0"

    -- | Checkout of the @kata-containers@ repo. Currently at release 3.23.0.
    kataContainersDerivation :: Text
    kataContainersDerivation = [__i|{fetchFromGitHub}:

                                    fetchFromGitHub {
                                      owner = "kata-containers";
                                      repo = "kata-containers";
                                      rev = "650ada7bcc8e47e44b55848765b0eb3ae9240454";
                                      sha256 = "sha256-h9Jsto2l1NhQEwIQoecT/D+yt/QbGosqH/l6NNzJOwk=";
                                    }
                                   |]

defaultKataContainersOptionsHelmChart :: FilePath -> KataContainersOptions
defaultKataContainersOptionsHelmChart helmBinary = KataContainersOptionsHelmChart {
  kataContainersHelmBinary = helmBinary
  , kataContainersHelmChart = "oci://ghcr.io/kata-containers/kata-deploy-charts/kata-deploy:3.23.0"
  , kataContainersHelmArgs = []
  }

kataContainers :: Label "kataContainers" KataContainersContext
kataContainers = Label
type HasKataContainersContext context = HasLabel context "kataContainers" KataContainersContext

type ContextWithKataContainers context =
  LabelValue "kataContainers" KataContainersContext
  :> context
