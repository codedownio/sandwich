{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.FindImages (
  findAllImages
  , findAllImages'
  ) where

import Control.Lens
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Text as T
import qualified Data.Yaml as Yaml
import Kubernetes.OpenAPI.Model as Kubernetes
import Kubernetes.OpenAPI.ModelLens as Kubernetes
import Relude
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson


-- | Find all image references in a chunk of YAML containing multiple sections
findAllImages :: Text -> [Text]
findAllImages = Relude.concatMap findAllImages' . T.splitOn "---\n"

-- | Find all image references in a single chunk of YAML
findAllImages' :: Text -> [Text]
findAllImages' (decode -> Right x@(V1Pod {v1PodKind=(Just "Pod")})) = maybe [] imagesFromPodSpec (v1PodSpec x)
findAllImages' (decode -> Right x@(V1Deployment {v1DeploymentKind=(Just "Deployment")})) = maybe [] imagesFromPodSpec maybePodSpec
  where
    maybePodSpec :: Maybe V1PodSpec
    maybePodSpec = x ^? (v1DeploymentSpecL . _Just . v1DeploymentSpecTemplateL . v1PodTemplateSpecSpecL . _Just)
findAllImages' (decode -> Right x@(V1StatefulSet {v1StatefulSetKind=(Just "StatefulSet")})) = maybe [] imagesFromPodSpec maybePodSpec
  where
    maybePodSpec :: Maybe V1PodSpec
    maybePodSpec = x ^? (v1StatefulSetSpecL . _Just . v1StatefulSetSpecTemplateL . v1PodTemplateSpecSpecL . _Just)
findAllImages' (decode -> Right x@(V1DaemonSet {v1DaemonSetKind=(Just "DaemonSet")})) = maybe [] imagesFromPodSpec maybePodSpec
  where
    maybePodSpec :: Maybe V1PodSpec
    maybePodSpec = x ^? (v1DaemonSetSpecL . _Just . v1DaemonSetSpecTemplateL . v1PodTemplateSpecSpecL . _Just)

-- Pick up images in MinIO "Tenant" CRDs
findAllImages' (decode -> Right x@(A.Object obj))
  | Just (A.String "Tenant") <- aesonLookup "kind" obj
    , Just (A.String img) <- x ^? (_Object . ix "spec" . _Object . ix "image") = [img]

findAllImages' _ = []

imagesFromPodSpec :: V1PodSpec -> [Text]
imagesFromPodSpec x = mapMaybe v1ContainerImage allContainers
  where
    allContainers = x ^. v1PodSpecContainersL <> fromMaybe [] (x ^. v1PodSpecInitContainersL)

decode :: FromJSON a => Text -> Either Yaml.ParseException a
decode = Yaml.decodeEither' . encodeUtf8
