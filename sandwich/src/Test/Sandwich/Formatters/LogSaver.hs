{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- | A simple formatter that saves all logs from the test to a file.

module Test.Sandwich.Formatters.LogSaver (
  defaultLogSaverFormatter
  , logSaverPath
  , logSaverLogLevel

  , LogPath(..)
  ) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import System.IO
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Util

data LogSaverFormatter = LogSaverFormatter {
  logSaverPath :: LogPath
  -- ^ Path under runRoot where logs will be saved. Should be a relative path.
  , logSaverLogLevel :: LogLevel
  -- ^ Minimum log level to save.
  }

data LogPath =
  LogPathRelativeToRunRoot FilePath
  -- ^ Interpret the path as relative to the test's run root. (If there is no run root, the logs won't be saved.)
  | LogPathAbsolute FilePath
  -- ^ Interpret the path as an absolute path.

defaultLogSaverFormatter :: LogSaverFormatter
defaultLogSaverFormatter = LogSaverFormatter {
  logSaverPath = LogPathRelativeToRunRoot "logs.txt"
  , logSaverLogLevel = LevelWarn
  }

instance Formatter LogSaverFormatter where
  runFormatter = runApp

runApp :: LogSaverFormatter -> [RunNode BaseContext] -> BaseContext -> IO ()
runApp lsf@(LogSaverFormatter {..}) rts bc = do
  let maybePath = case logSaverPath of
        LogPathAbsolute p -> Just p
        LogPathRelativeToRunRoot p -> case baseContextRunRoot bc of
          Nothing -> Nothing
          Just rr -> Just (rr </> p)

  whenJust maybePath $ \path ->
    withFile path AppendMode $ \h ->
      runReaderT (mapM_ run rts) (lsf, h)

run :: RunNode context -> ReaderT (LogSaverFormatter, Handle) IO ()
run node@(RunNodeIt {..}) = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  _ <- liftIO $ waitForTree node
  printLogs runTreeLogs
run node = do
  let RunNodeCommonWithStatus {..} = runNodeCommon node
  _ <- liftIO $ waitForTree node
  printLogs runTreeLogs

printLogs :: (MonadIO m, MonadReader (LogSaverFormatter, Handle) m, Foldable t) => TVar (t LogEntry) -> m ()
printLogs runTreeLogs = do
  (LogSaverFormatter {..}, h) <- ask
  logEntries <- liftIO $ readTVarIO runTreeLogs
  forM_ logEntries $ \(LogEntry {..}) ->
    when (logEntryLevel >= logSaverLogLevel) $
      liftIO $ BS8.hPutStr h $
        defaultLogStrBS logEntryLoc logEntrySource logEntryLevel logEntryStr

defaultLogStrBS :: Loc -> LogSource -> LogLevel -> LogStr -> BS8.ByteString
defaultLogStrBS a b c d = fromLogStr $ defaultLogStr a b c d
