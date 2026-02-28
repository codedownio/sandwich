module Test.Sandwich.Formatters.Socket.Server (
  socketServer
  ) where

import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.Time
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import Test.Sandwich.Formatters.Socket.Commands
import Test.Sandwich.Types.RunTree
import UnliftIO.Exception


-- | Bidirectional Unix socket server. Each client connection reads line-based
-- commands and sends back responses terminated by a line containing just ".".
socketServer :: FilePath -> [RunNode BaseContext] -> IO ()
socketServer socketPath rts = do
  -- Clean up any existing socket file
  removeFile socketPath `catch` \(_ :: IOError) -> return ()

  bracket (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrUnix socketPath)
    listen sock 5
    forever $ do
      (conn, _) <- accept sock
      void $ async $ handleConnection conn rts

handleConnection :: Socket -> [RunNode BaseContext] -> IO ()
handleConnection conn rts = do
  bufRef <- newIORef BS.empty
  handle (\(_ :: IOError) -> close conn) $ do
    sendAll conn "Connected to sandwich socket formatter. Type \"help\" for commands.\n\n> "
    forever $ do
      line <- readLine conn bufRef
      now <- getCurrentTime
      response <- handleCommand rts now (BS8.unpack (stripCR line))
      unless (null response) $ do
        sendAll conn (BS8.pack response)
        sendAll conn "\n"
      sendAll conn "> "

-- | Read a single line from the socket, buffering leftover bytes.
-- Returns the line without the trailing newline.
readLine :: Socket -> IORef BS.ByteString -> IO BS.ByteString
readLine conn bufRef = do
  buf <- readIORef bufRef
  case BS8.elemIndex '\n' buf of
    Just idx -> do
      let (line, rest) = BS.splitAt idx buf
      writeIORef bufRef (BS.drop 1 rest) -- skip the '\n'
      return line
    Nothing -> do
      chunk <- recv conn 4096
      when (BS.null chunk) $ throwIO (userError "connection closed")
      writeIORef bufRef (buf <> chunk)
      readLine conn bufRef

-- | Strip trailing carriage return (for clients that send \r\n)
stripCR :: BS.ByteString -> BS.ByteString
stripCR bs
  | BS.null bs = bs
  | BS8.last bs == '\r' = BS.init bs
  | otherwise = bs
