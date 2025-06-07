{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.Setup (
  setUpKindCluster
  , getNodes
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Relude
import System.Exit
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Contexts.Kubernetes.Waits
import UnliftIO.Environment
import UnliftIO.Process


setUpKindCluster :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => KubernetesClusterContext -> FilePath -> FilePath -> Maybe [(String, String)] -> Text -> m ()
setUpKindCluster kcc@(KubernetesClusterContext {..}) kindBinary kubectlBinary environmentToUse driver = do
  baseEnv <- maybe getEnvironment return environmentToUse
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
  let runWithKubeConfig cmd = createProcessWithLogging ((shell cmd) { env = Just env, delegate_ctlc = True })

  info [i|Installing ingress-nginx|]
  runWithKubeConfig [i|#{kubectlBinary} apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml|]
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
  -- void $ runWithKubeConfig [i|kubectl patch deployments -n ingress-nginx nginx-ingress-controller -p '{"spec":{"template":{"spec":{"containers":[{"name":"nginx-ingress-controller","ports":[{"containerPort":80,"hostPort":0},{"containerPort":443,"hostPort":0}]}],"nodeSelector":{"ingress-ready":"true"},"tolerations":[{"key":"node-role.kubernetes.io/master","operator":"Equal","effect":"NoSchedule"}]}}}}'|]
  info [i|Waiting for ingress-nginx|]
  flip runReaderT (LabelValue @"kubernetesCluster" kcc) $
    waitForPodsToExist "ingress-nginx" (M.singleton "app.kubernetes.io/component" "controller") 120.0 Nothing
  info [i|controller pod existed|]
  runWithKubeConfig [iii|#{kubectlBinary} wait pod
                         --namespace ingress-nginx
                         --for=condition=ready
                         --selector=app.kubernetes.io/component=controller
                         --timeout=300s|]
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- info [i|Installing metrics server using helm|]
  -- void $ runWithKubeConfig [i|helm repo add bitnami https://charts.bitnami.com/bitnami|]
  -- void $ runWithKubeConfig [i|helm install metrics-server-release bitnami/metrics-server|]

  info [i|Installing metrics server|]
  runWithKubeConfig [i|#{kubectlBinary} apply -f https://github.com/kubernetes-sigs/metrics-server/releases/download/v0.6.4/components.yaml|]
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)
  runWithKubeConfig [i|#{kubectlBinary} patch -n kube-system deployment metrics-server --type=json -p '[{"op":"add","path":"/spec/template/spec/containers/0/args/-","value":"--kubelet-insecure-tls"}]'|]
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  when (driver == "docker") $ do
    info [i|Fixing perms on /dev/fuse|] -- Needed on NixOS where it gets mounted 0600, don't know why
    nodes <- getNodes kcc kindBinary environmentToUse
    forM_ nodes $ \node -> do
      info [i|  (#{node}) Fixing /dev/fuse|]
      void $ readCreateProcess (shell [i|#{driver} exec "#{node}" chmod 0666 /dev/fuse|]) ""


getNodes :: MonadUnliftIO m => KubernetesClusterContext -> FilePath -> Maybe [(String, String)] -> m [Text]
getNodes (KubernetesClusterContext {..}) kindBinary environmentToUse = do
  baseEnv <- maybe getEnvironment return environmentToUse
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)
  ((words . toText) <$> (readCreateProcess ((shell [i|#{kindBinary} get nodes --name "#{kubernetesClusterName}"|]) { env = Just env }) ""))
