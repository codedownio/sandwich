{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Test.Sandwich.Formatters.TerminalUI.AttrMap where

import Brick
import Brick.Widgets.ProgressBar
import qualified Graphics.Vty as V
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

#if MIN_VERSION_brick(1,0,0)
mkAttrName :: String -> AttrName
mkAttrName = attrName
#else
import Data.String

mkAttrName :: String -> AttrName
mkAttrName = fromString
#endif


mainAttrMap :: AttrMap
mainAttrMap = attrMap V.defAttr [
  -- (listAttr, V.white `on` V.blue)
   -- (listSelectedAttr, V.blue `on` V.white)
  -- (listSelectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))
  -- (selectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))

  -- Top bar
  (visibilityThresholdNotSelectedAttr, fg midGray)
  , (visibilityThresholdSelectedAttr, fg solarizedBase2)

  -- Statuses
  -- , (notStartedAttr, fg V.)
  , (runningAttr, fg V.blue)
  , (pendingAttr, fg V.yellow)
  , (successAttr, fg V.green)
  , (failureAttr, fg V.red)
  , (totalAttr, fg solarizedCyan)

  -- Logging
  , (debugAttr, fg V.blue), (infoAttr, fg V.yellow), (warnAttr, fg V.red), (errorAttr, fg V.red), (otherAttr, V.defAttr)
  , (logTimestampAttr, fg midGray)
  , (logFilenameAttr, fg solarizedViolet)
  , (logModuleAttr, fg solarizedMagenta)
  , (logPackageAttr, fg solarizedGreen)
  , (logLineAttr, fg solarizedCyan)
  , (logChAttr, fg solarizedOrange)
  , (logFunctionAttr, fg solarizedMagenta)

  -- Progress bar
  , (progressCompleteAttr, bg (V.Color240 235))
  , (progressIncompleteAttr, bg (V.Color240 225))

  -- Main list
  , (toggleMarkerAttr, fg midGray)
  , (openMarkerAttr, fg midGray)
  , (visibilityThresholdIndicatorMutedAttr, fg $ grayAt 50)
  , (visibilityThresholdIndicatorAttr, fg $ grayAt 150)

  -- Hotkey stuff
  , (hotkeyAttr, fg V.blue)
  , (disabledHotkeyAttr, fg midGray)
  , (hotkeyMessageAttr, fg brightWhite)
  , (disabledHotkeyMessageAttr, fg brightGray)

  -- Exceptions and pretty printing
  , (expectedAttr, fg midWhite)
  , (sawAttr, fg midWhite)
  , (integerAttr, fg solarizedMagenta)
  , (floatAttr, fg solarizedMagenta)
  , (charAttr, fg solarizedCyan)
  , (stringAttr, fg solarizedYellow)
  , (dateAttr, fg solarizedBase2)
  , (timeAttr, fg solarizedBase1)
  , (quoteAttr, fg solarizedBase1)
  , (slashAttr, fg solarizedViolet)
  , (negAttr, fg solarizedViolet)
  , (listBracketAttr, fg solarizedOrange) -- TODO: make green?
  , (tupleBracketAttr, fg solarizedGreen)
  , (braceAttr, fg solarizedGreen)
  , (ellipsesAttr, fg solarizedBase0)
  , (recordNameAttr, fg solarizedRed)
  , (fieldNameAttr, fg solarizedYellow)
  , (constructorNameAttr, fg solarizedViolet)
  ]

-- selectedAttr :: AttrName
-- selectedAttr = "list_line_selected"

visibilityThresholdNotSelectedAttr :: AttrName
visibilityThresholdNotSelectedAttr = mkAttrName "visibility_threshold_not_selected"

visibilityThresholdSelectedAttr :: AttrName
visibilityThresholdSelectedAttr = mkAttrName "visibility_threshold_selected"

runningAttr :: AttrName
runningAttr = mkAttrName "running"

notStartedAttr :: AttrName
notStartedAttr = mkAttrName "not_started"

pendingAttr :: AttrName
pendingAttr = mkAttrName "pending"

totalAttr :: AttrName
totalAttr = mkAttrName "total"

successAttr :: AttrName
successAttr = mkAttrName "success"

failureAttr :: AttrName
failureAttr = mkAttrName "failure"

toggleMarkerAttr :: AttrName
toggleMarkerAttr = mkAttrName "toggleMarker"

openMarkerAttr :: AttrName
openMarkerAttr = mkAttrName "openMarker"

visibilityThresholdIndicatorAttr :: AttrName
visibilityThresholdIndicatorAttr = mkAttrName "visibilityThresholdIndicator"

visibilityThresholdIndicatorMutedAttr :: AttrName
visibilityThresholdIndicatorMutedAttr = mkAttrName "visibilityThresholdMutedIndicator"

hotkeyAttr, disabledHotkeyAttr, hotkeyMessageAttr, disabledHotkeyMessageAttr :: AttrName
hotkeyAttr = mkAttrName "hotkey"
disabledHotkeyAttr = mkAttrName "disableHotkey"
hotkeyMessageAttr = mkAttrName "hotkeyMessage"
disabledHotkeyMessageAttr = mkAttrName "disabledHotkeyMessage"

chooseAttr :: Status -> AttrName
chooseAttr NotStarted = notStartedAttr
chooseAttr (Running {}) = runningAttr
chooseAttr (Done _ _ (Success {})) = successAttr
chooseAttr (Done _ _ (Failure (Pending {}))) = pendingAttr
chooseAttr (Done _ _ (Failure {})) = failureAttr
chooseAttr (Done _ _ DryRun) = notStartedAttr
chooseAttr (Done _ _ Cancelled) = failureAttr

-- * Logging and callstacks

debugAttr, infoAttr, warnAttr, errorAttr, otherAttr :: AttrName
debugAttr = attrName"log_debug"
infoAttr = attrName"log_info"
warnAttr = attrName"log_warn"
errorAttr = attrName"log_error"
otherAttr = mkAttrName "log_other"

logTimestampAttr :: AttrName
logTimestampAttr = mkAttrName "log_timestamp"

logFilenameAttr, logModuleAttr, logPackageAttr, logLineAttr, logChAttr :: AttrName
logFilenameAttr = mkAttrName "logFilename"
logModuleAttr = mkAttrName "logModule"
logPackageAttr = mkAttrName "logPackage"
logLineAttr = mkAttrName "logLine"
logChAttr = mkAttrName "logCh"
logFunctionAttr = mkAttrName "logFunction"

-- * Exceptions and pretty printing

expectedAttr, sawAttr :: AttrName
expectedAttr = mkAttrName "expected"
sawAttr = mkAttrName "saw"

integerAttr, timeAttr, dateAttr, stringAttr, charAttr, floatAttr, quoteAttr, slashAttr, negAttr :: AttrName
listBracketAttr, tupleBracketAttr, braceAttr, ellipsesAttr, recordNameAttr, fieldNameAttr, constructorNameAttr :: AttrName
integerAttr = mkAttrName "integer"
floatAttr = mkAttrName "float"
charAttr = mkAttrName "char"
stringAttr = mkAttrName "string"
dateAttr = mkAttrName "date"
timeAttr = mkAttrName "time"
quoteAttr = mkAttrName "quote"
slashAttr = mkAttrName "slash"
negAttr = mkAttrName "neg"
listBracketAttr = mkAttrName "listBracket"
tupleBracketAttr = mkAttrName "tupleBracket"
braceAttr = mkAttrName "brace"
ellipsesAttr = mkAttrName "ellipses"
recordNameAttr = mkAttrName "recordName"
fieldNameAttr = mkAttrName "fieldName"
constructorNameAttr = mkAttrName "fieldName"

-- * Colors

solarizedBase03 = V.rgbColor 0x00 0x2b 0x36
solarizedBase02 = V.rgbColor 0x07 0x36 0x42
solarizedBase01 = V.rgbColor 0x58 0x6e 0x75
solarizedbase00 = V.rgbColor 0x65 0x7b 0x83
solarizedBase0 = V.rgbColor 0x83 0x94 0x96
solarizedBase1 = V.rgbColor 0x93 0xa1 0xa1
solarizedBase2 = V.rgbColor 0xee 0xe8 0xd5
solarizedBase3 = V.rgbColor 0xfd 0xf6 0xe3
solarizedYellow = V.rgbColor 0xb5 0x89 0x00
solarizedOrange = V.rgbColor 0xcb 0x4b 0x16
solarizedRed = V.rgbColor 0xdc 0x32 0x2f
solarizedMagenta = V.rgbColor 0xd3 0x36 0x82
solarizedViolet = V.rgbColor 0x6c 0x71 0xc4
solarizedBlue = V.rgbColor 0x26 0x8b 0xd2
solarizedCyan = V.rgbColor 0x2a 0xa1 0x98
solarizedGreen = V.rgbColor 0x85 0x99 0x00

midGray = grayAt 50
brightGray = grayAt 80
midWhite = grayAt 140
brightWhite = grayAt 200

grayAt level = V.rgbColor level level level
-- grayAt level = V.Color240 $ V.rgbColorToColor240 level level level
