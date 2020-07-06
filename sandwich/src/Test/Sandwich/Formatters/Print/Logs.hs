{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.Print.Logs where

import Control.Monad.Logger
import Data.String.Interpolate
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Types.RunTree


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

  p (show logEntryStr)

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
