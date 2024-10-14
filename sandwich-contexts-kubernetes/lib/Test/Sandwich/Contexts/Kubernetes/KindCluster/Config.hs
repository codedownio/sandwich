{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.Config where

import Data.Aeson as A
import Data.Aeson.TH as A
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Vector as V
import Relude


data ExtraPortMapping = ExtraPortMapping {
  containerPort :: Int16
  , hostPort :: Int16

  -- | Set the bind address on the host
  -- 0.0.0.0 is the current default
  , listenAddress :: Maybe String

  -- | Set the protocol to one of TCP, UDP, SCTP.
  --  TCP is the default
  , protocol :: Maybe String
  }
deriveToJSON A.defaultOptions ''ExtraPortMapping

data ExtraMount = ExtraMount {
  hostPath :: String
  , containerPath :: String
  -- | If set, the mount is read-only.
  -- default false
  , readOnly :: Maybe Bool
  -- | If set, the mount needs SELinux relabeling.
  -- default false
  , selinuxRelabel :: Maybe Bool

  -- | Set propagation mode (None, HostToContainer or Bidirectional).
  -- See https://kubernetes.io/docs/concepts/storage/volumes/#mount-propagation.
  --
  -- WARNING: You very likely do not need this field.
  --
  -- This field controls propagation of *additional* mounts created
  -- at runtime underneath this mount.
  --
  -- On MacOS with Docker Desktop, if the mount is from macOS and not the
  -- docker desktop VM, you cannot use this field. You can use it for
  -- mounts to the linux VM.
  , propagation :: Maybe String
  }
deriveToJSON A.defaultOptions ''ExtraMount

kindConfig :: Int -> Map Text Text -> [ExtraPortMapping] -> [ExtraMount] -> A.Value
kindConfig numNodes _containerLabels extraPortMappings extraMounts = A.object [
  ("kind", A.String "Cluster")
  , ("apiVersion", A.String "kind.x-k8s.io/v1alpha4")
  , ("nodes", A.Array (V.fromList nodes))
  ]
  where
    nodes = mkNode "control-plane" : (L.replicate (numNodes - 1) (mkNode "worker"))

    mkNode :: Text -> A.Value
    mkNode role = A.object ([
      ("role", A.String role)
      ]
      <> if role == "control-plane" then [("kubeadmConfigPatches", A.Array (V.fromList [A.String extraPatches]))] else []
      <> if L.null extraPortMappings then [] else [("extraPortMappings", A.Array (V.fromList (fmap A.toJSON extraPortMappings)))]
      <> if L.null extraMounts then [] else [("extraMounts", A.Array (V.fromList (fmap A.toJSON extraMounts)))]
      )

    extraPatches = [__i|kind: InitConfiguration
                        nodeRegistration:
                          kubeletExtraArgs:
                            node-labels: "ingress-ready=true"
                            authorization-mode: "AlwaysAllow"
                            streaming-connection-idle-timeout: "0"
                       |]


-- Note: here's how to provide an extra container registry:
-- containerdConfigPatches:
-- - |-
--   [plugins."io.containerd.grpc.v1.cri".registry.mirrors."#{registryHostname}:5000"]
--     endpoint = ["http://#{registryHostname}:5000"]
