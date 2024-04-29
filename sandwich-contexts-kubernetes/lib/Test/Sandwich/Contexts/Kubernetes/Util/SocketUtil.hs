module Test.Sandwich.Contexts.Kubernetes.Util.SocketUtil (
  isPortOpen
  , simpleSockAddr
  ) where

-- Taken from
-- https://stackoverflow.com/questions/39139787/i-want-to-check-whether-or-not-a-certain-port-is-open-haskell
-- https://gist.github.com/nh2/0a1442eb71ec0405a1e3ce83a467dfde#file-socketutils-hs

import Foreign.C.Error (Errno(..), eCONNREFUSED)
import GHC.IO.Exception (IOException(..))
import Network.Socket (Family(AF_INET), PortNumber, SocketType(Stream), SockAddr(SockAddrInet), socket, connect, close', tupleToHostAddress)
import Relude
import UnliftIO.Exception

-- | Checks whether @connect()@ to a given TCPv4 `SockAddr` succeeds or
-- returns `eCONNREFUSED`.
--
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
isPortOpen :: SockAddr -> IO Bool
isPortOpen sockAddr = do
  bracket (socket AF_INET Stream 6 {- TCP -}) close' $ \sock -> do
    res <- try $ connect sock sockAddr
    case res of
      Right () -> return True
      Left e ->
        if (Errno <$> ioe_errno e) == Just eCONNREFUSED
          then return False
          else throwIO e


-- | Creates a `SockAttr` from host IP and port number.
--
-- Example:
-- > simpleSockAddr (127,0,0,1) 8000
simpleSockAddr :: (Word8, Word8, Word8, Word8) -> PortNumber -> SockAddr
simpleSockAddr addr port = SockAddrInet port (tupleToHostAddress addr)


-- Example usage:
-- > isPortOpen (simpleSockAddr (127,0,0,1) 8000)
-- True
