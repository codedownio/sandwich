-- |

module Test.Sandwich.Interpreters.RunTree.Logging (
  logToMemory
  , logToMemoryAndFile
  , LogFn
  , LogEntryFormatter
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS8
import Data.Sequence
import Data.Time.Clock
import System.IO
import Test.Sandwich.Types.RunTree

logToMemory :: Maybe LogLevel -> TVar (Seq LogEntry) -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToMemory Nothing _ _ _ _ _ = return ()
logToMemory (Just minLevel) logs loc logSrc logLevel logStr =
  when (logLevel >= minLevel) $ do
    ts <- getCurrentTime
    atomically $ modifyTVar logs (|> LogEntry ts loc logSrc logLevel logStr)

logToMemoryAndFile :: Maybe LogLevel -> Maybe LogLevel -> LogEntryFormatter -> TVar (Seq LogEntry) -> Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToMemoryAndFile maybeMemLogLevel maybeSavedLogLevel formatter logs h loc logSrc logLevel logStr = do
  case maybeMemLogLevel of
    Just x | x <= logLevel -> do
      ts <- getCurrentTime
      atomically $ modifyTVar logs (|> LogEntry ts loc logSrc logLevel logStr)
    _ -> return ()

  case maybeSavedLogLevel of
    Just x | x <= logLevel ->
      BS8.hPutStr h $ formatter loc logSrc logLevel logStr
    _ -> return ()
