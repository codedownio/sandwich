{-# LANGUAGE BangPatterns #-}

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

forceSeq :: Seq LogEntry -> Seq LogEntry
forceSeq s = foldl' (\_ e -> e `seq` ()) () s `seq` s

logToMemory :: Maybe LogLevel -> TVar (Seq LogEntry) -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToMemory Nothing _ _ _ _ _ = return ()
logToMemory (Just minLevel) logs loc logSrc logLevel logStr =
  when (logLevel >= minLevel) $ do
    ts <- getCurrentTime
    let !bs = fromLogStr logStr
    let !entry = LogEntry ts loc logSrc logLevel bs
    atomically $ modifyTVar' logs (\s -> let s' = s |> entry in forceSeq s' `seq` s')

logToMemoryAndFile :: Maybe LogLevel -> Maybe LogLevel -> LogEntryFormatter -> TVar (Seq LogEntry) -> Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToMemoryAndFile maybeMemLogLevel maybeSavedLogLevel formatter logs h loc logSrc logLevel logStr = do
  let !bs = fromLogStr logStr
  maybeTs <- case maybeMemLogLevel of
    Just x | x <= logLevel -> do
      ts <- getCurrentTime
      let !entry = LogEntry ts loc logSrc logLevel bs
      atomically $ modifyTVar' logs (\s -> let s' = s |> entry in forceSeq s' `seq` s')
      return $ Just ts
    _ -> return Nothing

  case maybeSavedLogLevel of
    Just x | x <= logLevel -> do
      ts <- maybe getCurrentTime return maybeTs
      BS8.hPutStr h $ formatter ts loc logSrc logLevel bs
    _ -> return ()
