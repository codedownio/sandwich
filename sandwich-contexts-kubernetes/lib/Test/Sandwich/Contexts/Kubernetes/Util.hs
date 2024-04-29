
module Test.Sandwich.Contexts.Kubernetes.Util where

import qualified Data.List as L
import Data.String.Interpolate
import Network.HostName (HostName)
import Network.Socket (PortNumber)
import Network.URI
import Relude
import Safe
import Test.Sandwich


parseHostnameAndPort :: (MonadIO m) => URI -> m (HostName, PortNumber)
parseHostnameAndPort uri = case uriAuthority uri of
  Nothing -> expectationFailure [i|Server URI didn't have an authority: #{uri}|]
  Just (URIAuth {..}) -> case readMay (L.drop 1 uriPort) of
    Nothing -> expectationFailure [i|Couldn't read port: #{uriPort}|]
    Just x -> pure (uriRegName, x)
