-- | The socket formatter creates a Unix domain socket that accepts interactive,
-- line-based commands to query the live test tree state.
--
-- This is a "secondary formatter," i.e. one that can run in the background while
-- a "primary formatter" (such as the TerminalUI or Print formatters) runs in the
-- foreground.
--
-- The socket server stays alive for the entire duration of the test executable,
-- so you can query it even after tests have completed (until 'finalizeFormatter'
-- cleans it up).
--
-- Connect with @socat - UNIX-CONNECT:\<run-root\>/socket.sock@ and type
-- @help@ to see available commands.

module Test.Sandwich.Formatters.Socket (
  defaultSocketFormatter
  , SocketFormatter(..)
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable
import System.FilePath
import Test.Sandwich.Formatters.Socket.Server
import Test.Sandwich.Interpreters.RunTree.Util (waitForTree)
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree


data SocketFormatter = SocketFormatter {
  socketFormatterPath :: Maybe FilePath
  -- ^ Socket path. Nothing = \<run-root\>/socket.sock
  , socketFormatterServerAsync :: IORef (Maybe (Async ()))
  -- ^ Internal: handle to the running server thread, used for cleanup.
  , socketFormatterLogBroadcast :: TChan (Int, String, LogEntry)
  -- ^ Broadcast channel for streaming logs to connected clients.
  , socketFormatterEventBroadcast :: TChan NodeEvent
  -- ^ Broadcast channel for streaming node lifecycle events to connected clients.
  } deriving (Typeable)

instance Show SocketFormatter where
  show (SocketFormatter {socketFormatterPath}) =
    "SocketFormatter {socketFormatterPath = " <> show socketFormatterPath <> "}"

defaultSocketFormatter :: IO SocketFormatter
defaultSocketFormatter = do
  ref <- newIORef Nothing
  chan <- newBroadcastTChanIO
  eventChan <- newBroadcastTChanIO
  return SocketFormatter {
    socketFormatterPath = Nothing
    , socketFormatterServerAsync = ref
    , socketFormatterLogBroadcast = chan
    , socketFormatterEventBroadcast = eventChan
    }

instance Formatter SocketFormatter where
  formatterName _ = "socket-formatter"
  runFormatter = run
  finalizeFormatter sf _ _ = liftIO $ do
    mAsync <- readIORef (socketFormatterServerAsync sf)
    case mAsync of
      Nothing -> return ()
      Just a -> cancel a

run :: (MonadIO m) => SocketFormatter -> [RunNode BaseContext] -> Maybe (CommandLineOptions ()) -> BaseContext -> m ()
run (SocketFormatter {..}) rts _maybeCommandLineOptions bc = do
  case resolveSocketPath of
    Nothing -> return ()
    Just path -> liftIO $ do
      a <- async (socketServer path rts socketFormatterLogBroadcast socketFormatterEventBroadcast)
      writeIORef socketFormatterServerAsync (Just a)
      -- Block until all tests complete
      mapM_ waitForTree rts
  where
    resolveSocketPath = case socketFormatterPath of
      Just p -> Just p
      Nothing -> case baseContextRunRoot bc of
        Just runRoot -> Just (runRoot </> "socket.sock")
        Nothing -> Nothing
