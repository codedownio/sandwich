
module Test.Sandwich.Contexts.Types where

import Network.Socket
import Relude


data NetworkAddress =
  NetworkAddressTCP { networkAddressTcpHostname :: String
                    , networkAddressTcpPort :: PortNumber }
  | NetworkAddressUnix { networkAddressUnixPath :: String }
  deriving (Show, Eq)
