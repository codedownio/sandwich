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
import Data.Aeson as A
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Vector as V
import Relude hiding (withFile)
import System.Exit
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Images (loadImage')
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import UnliftIO.Environment
import UnliftIO.IO (withFile)
import UnliftIO.Process
import UnliftIO.Temporary


data LonghornContext = LonghornContext {
  longhornOptions :: LonghornOptions
  } deriving (Show)

data LonghornOptions = LonghornOptions {
  longhornFoo :: ()
  } deriving (Show)
defaultLonghornOptions :: LonghornOptions
defaultLonghornOptions = LonghornOptions {
  longhornFoo = ()
  }

longhorn :: Label "longhorn" LonghornContext
longhorn = Label
type HasLonghornContext context = HasLabel context "longhorn" LonghornContext

introduceLonghorn :: (
  HasBaseContext context, HasKubernetesClusterContext context, MonadUnliftIO m
  ) =>Text -> LonghornOptions -> SpecFree (LabelValue "longhorn" LonghornContext :> context) m () -> SpecFree context m ()
introduceLonghorn namespace options = introduceWith "introduce Longhorn" longhorn (void . withLonghorn namespace options)

withLonghorn :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m, HasBaseContextMonad context m, HasKubernetesClusterContext context
  ) => Text -> LonghornOptions -> (LonghornContext -> m a) -> m a
withLonghorn namespace options action = do
  kcc <- getContext kubernetesCluster
  withLonghorn' kcc namespace options action

withLonghorn' :: forall context m a. (
  HasCallStack, MonadFail m, MonadLoggerIO m, MonadUnliftIO m, HasBaseContextMonad context m
  ) => KubernetesClusterContext -> Text -> LonghornOptions -> (LonghornContext -> m a) -> m a
withLonghorn' kcc@(KubernetesClusterContext {kubernetesClusterKubeConfigPath}) namespace options action = do
  baseEnv <- getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  undefined
