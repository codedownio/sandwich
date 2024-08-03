

module Test.Sandwich.Contexts.Kubernetes.MinioS3Server.Parsing (
  parseMinioUserAndPassword
  , transformKustomizeChunks
  ) where

import Control.Lens
import Data.Aeson (FromJSON)
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.Map as M
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Yaml as Yaml
import Kubernetes.OpenAPI.Model as Kubernetes
import Relude
import Safe (headMay)
import Test.Sandwich.Contexts.Kubernetes.Util.Aeson
import Text.Regex.TDFA


parseMinioUserAndPassword :: Text -> Maybe (Text, Text)
parseMinioUserAndPassword txt = case (userValues, passwordValues) of
  (Just (_before, _fullMatch, _after, [user]), Just (_, _, _, [password])) -> Just (user, toText password)
  _ -> Nothing
  where
    userValues :: Maybe (Text, Text, Text, [Text]) = txt =~~ ([i|MINIO_ROOT_USER="([^"]*)"|] :: Text)
    passwordValues :: Maybe (Text, Text, Text, [Text]) = txt =~~ ([i|MINIO_ROOT_PASSWORD="([^"]*)"|] :: Text)

-- testInput :: Text
-- testInput = [__i|export MINIO_ROOT_USER="WXSTFUWIRS04LMGIMJGV"
--                  export MINIO_ROOT_PASSWORD="NCDCfTaiXcGHq8QRfSaXMAWOXgdrhpGwPSkoYMWf"|]

transformKustomizeChunks :: String -> String -> [Text] -> Either String ((Text, Text), Text)
transformKustomizeChunks namespace deploymentName initialChunks = do
  userAndPassword <- getUserAndPassword initialChunks

  return (userAndPassword, finalYaml)

  where
    finalYaml = initialChunks
              -- Don't include a kind: Namespace
              & Relude.filter (not . isNamespace)

              -- Set metadata.namespace on all values
              & fmap (setMetaNamespace namespace)

              -- Disable TLS
              & fmap disableTLS

              -- Set deployment name
              & fmap (setDeploymentNameAndPoolSize deploymentName)

              -- Combine everything into multi-document Yaml
              & T.intercalate "---\n"

getUserAndPassword :: [Text] -> Either String (Text, Text)
getUserAndPassword chunks = case headMay (mapMaybe getUserAndPassword' chunks) of
  Nothing -> Left "Couldn't find user/password YAML."
  Just x -> Right x
  where
    getUserAndPassword' :: Text -> Maybe (Text, Text)
    getUserAndPassword' (decode -> Right (V1Secret {v1SecretMetadata=(Just (V1ObjectMeta {v1ObjectMetaName=(Just "storage-configuration")}))
                                                   , v1SecretStringData=(Just (M.lookup "config.env" -> Just t))
                                                   }))
      = parseMinioUserAndPassword t
    getUserAndPassword' _ = Nothing

isNamespace :: Text -> Bool
isNamespace (decode -> Right (A.Object (aesonLookup "kind" -> Just (A.String "Namespace")))) = True
isNamespace _ = False

setMetaNamespace :: String -> Text -> Text
setMetaNamespace namespace (decode -> Right (A.Object obj1@(aesonLookup "metadata" -> Just (A.Object obj2@(aesonLookup "namespace" -> Just (A.String _)))))) =
  decodeUtf8 (Yaml.encode obj1')
  where
    obj1' :: A.Value
    obj1' = A.Object (aesonInsert "metadata" obj2' obj1)

    obj2' :: A.Value
    obj2' = A.Object (aesonInsert "namespace" (A.String (toText namespace)) obj2)
setMetaNamespace _ t = t

-- Do the steps to disable TLS in the tenant CRD.
-- See https://min.io/docs/minio/kubernetes/upstream/reference/operator-crd.html#tenantspec
disableTLS :: Text -> Text
disableTLS (decode -> Right x@(A.Object (aesonLookup "kind" -> Just (A.String "Tenant")))) = decodeUtf8 (Yaml.encode x')
  where
    x' = x
       & set (_Object . ix "spec" . _Object . ix "requestAutoCert") (A.Bool False)
       & set (_Object . ix "spec" . _Object . at "externalCertSecret") Nothing
disableTLS t = t

setDeploymentNameAndPoolSize :: String -> Text -> Text
setDeploymentNameAndPoolSize deploymentName (decode -> Right x@(A.Object (aesonLookup "kind" -> Just (A.String "Tenant")))) = decodeUtf8 (Yaml.encode x')
  where
    x' = x
       & set (_Object . ix "metadata" . _Object . ix "name") (A.String (toText deploymentName))
       & set (_Object . ix "spec" . _Object . ix "pools" . _Array . ix 0 . _Object . ix "servers") (A.Number 1)
setDeploymentNameAndPoolSize _ t = t

decode :: FromJSON a => Text -> Either Yaml.ParseException a
decode = Yaml.decodeEither' . encodeUtf8
