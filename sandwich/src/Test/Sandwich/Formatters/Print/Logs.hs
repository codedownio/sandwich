{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Sandwich.Formatters.Print.Logs where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import Data.String.Interpolate
import System.IO
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.RunTree

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad
#endif


printLogs :: (MonadIO m, MonadReader (PrintFormatter, Int, Handle) m, Foldable t) => TVar (t LogEntry) -> m ()
printLogs runTreeLogs = do
  (asks (printFormatterLogLevel . fst3)) >>= \case
    Nothing -> return ()
    Just logLevel -> do
      logEntries <- liftIO $ readTVarIO runTreeLogs
      withBumpIndent $
        forM_ logEntries $ \entry ->
          when (logEntryLevel entry >= logLevel) $ printLogEntry entry


printLogEntry :: (
  MonadReader (PrintFormatter, Int, Handle) m, MonadIO m
  ) => LogEntry -> m ()
printLogEntry (LogEntry {..}) = do
  pic logTimestampColor (show logEntryTime)

  case logEntryLevel of
    LevelDebug -> pc debugColor " (DEBUG) "
    LevelInfo -> pc infoColor " (INFO) "
    LevelWarn -> pc warnColor " (WARN) "
    LevelError -> pc errorColor " (ERROR) "
    LevelOther x -> pc infoColor [i| #{x} |]

  let Loc {loc_start=(line, ch), ..} = logEntryLoc
  p "["
  pc logFilenameColor loc_filename
  p ":"
  pc logLineColor (show line)
  p ":"
  pc logChColor (show ch)
  p "] "

  p (BS8.unpack logEntryStr)

  p "\n"


debugColor = solarizedBlue
infoColor = solarizedYellow
warnColor = solarizedRed
errorColor = solarizedRed
otherColor = solarizedYellow

logFilenameColor = solarizedViolet
logModuleColor = solarizedMagenta
logPackageColor = solarizedGreen
logLineColor = solarizedCyan
logChColor = solarizedOrange
logFunctionColor = solarizedBlue

logTimestampColor = midGray
