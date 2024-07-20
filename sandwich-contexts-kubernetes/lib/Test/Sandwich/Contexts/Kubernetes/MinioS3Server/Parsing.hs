

module Test.Sandwich.Contexts.Kubernetes.MinioS3Server.Parsing (
  parseMinioUserAndPassword
  ) where

import Data.String.Interpolate
import Data.Text
import Relude
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
