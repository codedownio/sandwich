{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sandwich.Contexts.Kubernetes.KindCluster.Config where

import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Relude


kindConfig :: Map Text Text -> Maybe Text -> Int -> Maybe FilePath -> Text
kindConfig _containerLabels _maybeRegistryHostname numNodes maybeBinaryCachePath = [i|
kind: Cluster
apiVersion: kind.x-k8s.io/v1alpha4
nodes:
#{mkRole "control-plane"}
#{T.intercalate "\n" $ L.replicate (numNodes - 1) (mkRole "worker")}
|]
   where
     mkRole :: Text -> Text
     mkRole name = [i|- role: #{name}
#{extraMounts}
#{controlPlaneSetup name}
|]

     extraMounts :: Text = case maybeBinaryCachePath of
       Nothing -> ""
       Just p -> [i|
  extraMounts:
  - hostPath: #{p}
    containerPath: /binary-cache
    readOnly: false
    propagation: HostToContainer
|]

     controlPlaneSetup :: Text -> Text
     controlPlaneSetup name = case name of
       "control-plane" -> [i|  kubeadmConfigPatches:
  - |
    kind: InitConfiguration
    nodeRegistration:
      kubeletExtraArgs:
        node-labels: "ingress-ready=true"
        authorization-mode: "AlwaysAllow"
        streaming-connection-idle-timeout: "0"
  extraPortMappings:
  - containerPort: 80
    hostPort: 0
|]
       _ -> ""


--      localRegistryConfig = case maybeRegistryHostname of Nothing -> ""; Just registryHostname -> [i|
-- containerdConfigPatches:
-- - |-
--   [plugins."io.containerd.grpc.v1.cri".registry.mirrors."#{registryHostname}:5000"]
--     endpoint = ["http://#{registryHostname}:5000"]
-- |]
