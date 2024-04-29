

module Test.Sandwich.Contexts.Kubernetes.MinioS3Server.Parsing (
  parseMinioUserAndPassword
  ) where

import Control.Lens
import Control.Lens.Regex.Text
import Data.Text
import Relude


parseMinioUserAndPassword :: Text -> Maybe (Text, Text)
parseMinioUserAndPassword txt = case (userValues, passwordValues) of
  ([[user]], [[password]]) -> Just (user, password)
  _ -> Nothing
  where
    userValues = txt ^.. [regex|MINIO_ROOT_USER="([^\"]*)"|] . groups
    passwordValues = txt ^.. [regex|MINIO_ROOT_PASSWORD="([^\"]*)"|] . groups

-- testInput :: Text
-- testInput = [__i|export MINIO_ROOT_USER="WXSTFUWIRS04LMGIMJGV"
--                  export MINIO_ROOT_PASSWORD="NCDCfTaiXcGHq8QRfSaXMAWOXgdrhpGwPSkoYMWf"|]
