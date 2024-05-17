
module Test.Sandwich.Contexts.Types.Network where

import Network.Socket
import Relude


data NetworkAddress =
  NetworkAddressTCP { networkAddressTcpHostname :: String
                    , networkAddressTcpPort :: PortNumber }
  | NetworkAddressUnix { networkAddressUnixPath :: String }
  deriving (Show, Eq)
