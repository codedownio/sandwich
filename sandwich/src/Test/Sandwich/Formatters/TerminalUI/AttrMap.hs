{-# LANGUAGE OverloadedStrings #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.AttrMap where

import Brick
import Brick.Widgets.List
import Brick.Widgets.ProgressBar
import qualified Graphics.Vty as V
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


mainAttrMap :: AttrMap
mainAttrMap = attrMap V.defAttr [
  -- (listAttr, V.white `on` V.blue)
   -- (listSelectedAttr, V.blue `on` V.white)
  -- (listSelectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))

  (selectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))

  -- Statuses
  -- , (notStartedAttr, fg V.)
  , (runningAttr, fg V.blue), (pendingAttr, fg V.yellow), (successAttr, fg V.green), (failureAttr, fg V.red)

  -- Logging
  , (debugAttr, fg V.blue), (infoAttr, fg V.yellow), (warnAttr, fg V.red), (errorAttr, fg V.red), (otherAttr, V.defAttr)
  , (logTimestampAttr, fg (grayAt 50))
  , (logFilenameAttr, fg V.blue), (logModuleAttr, fg V.magenta), (logLineAttr, fg V.cyan), (logChAttr, fg V.magenta)

  -- Progress bar
  , (progressCompleteAttr, bg (V.Color240 235))
  , (progressIncompleteAttr, bg (V.Color240 225))

  -- Main list
  , (toggleMarkerAttr, fg (grayAt 50))

  -- Hotkey stuff
  , (hotkeyAttr, fg V.blue)
  , (disabledHotkeyAttr, fg (grayAt 50))
  , (hotkeyMessageAttr, fg (grayAt 200))
  , (disabledHotkeyMessageAttr, fg (grayAt 80))

  -- Pretty printing
  , (integerAttr, fg solarizedMagenta)
  , (floatAttr, fg solarizedMagenta)
  , (charAttr, fg solarizedCyan)
  , (stringAttr, fg solarizedYellow)
  , (dateAttr, fg solarizedBase2)
  , (timeAttr, fg solarizedBase3)
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

grayAt level = V.Color240 $ V.rgbColorToColor240 level level level

selectedAttr :: AttrName
selectedAttr = "list_line_selected"

runningAttr :: AttrName
runningAttr = "running"

notStartedAttr :: AttrName
notStartedAttr = "not_started"

pendingAttr :: AttrName
pendingAttr = "pending"

successAttr :: AttrName
successAttr = "success"

failureAttr :: AttrName
failureAttr = "failure"

toggleMarkerAttr :: AttrName
toggleMarkerAttr = "toggleMarker"

hotkeyAttr, disabledHotkeyAttr, hotkeyMessageAttr, disabledHotkeyMessageAttr :: AttrName
hotkeyAttr = "hotkey"
disabledHotkeyAttr = "disableHotkey"
hotkeyMessageAttr = "hotkeyMessage"
disabledHotkeyMessageAttr = "disabledHotkeyMessage"

chooseAttr :: Status -> AttrName
chooseAttr NotStarted = notStartedAttr
chooseAttr (Running {}) = runningAttr
chooseAttr (Done _ _ (Success {})) = successAttr
chooseAttr (Done _ _ (Failure (Pending {}))) = pendingAttr
chooseAttr (Done _ _ (Failure {})) = failureAttr

-- * Logging

debugAttr, infoAttr, warnAttr, errorAttr, otherAttr :: AttrName
debugAttr = "log_debug"
infoAttr = "log_info"
warnAttr = "log_warn"
errorAttr = "log_error"
otherAttr = "log_other"

logTimestampAttr :: AttrName
logTimestampAttr = "log_timestamp"

logFilenameAttr, logModuleAttr, logLineAttr, logChAttr :: AttrName
logFilenameAttr = "log_filename"
logModuleAttr = "log_module"
logLineAttr = "log_line"
logChAttr = "log_ch"


-- * Exceptions and pretty printing

expectedAttr, gotAttr :: AttrName
expectedAttr = "expected"
gotAttr = "got"

integerAttr, timeAttr, dateAttr, stringAttr, charAttr, floatAttr, quoteAttr, slashAttr, negAttr :: AttrName
listBracketAttr, tupleBracketAttr, braceAttr, ellipsesAttr, recordNameAttr, fieldNameAttr, constructorNameAttr :: AttrName
integerAttr = "integer"
floatAttr = "float"
charAttr = "char"
stringAttr = "string"
dateAttr = "date"
timeAttr = "time"
quoteAttr = "quote"
slashAttr = "slash"
negAttr = "neg"
listBracketAttr = "listBracket"
tupleBracketAttr = "tupleBracket"
braceAttr = "brace"
ellipsesAttr = "ellipses"
recordNameAttr = "recordName"
fieldNameAttr = "fieldName"
constructorNameAttr = "fieldName"

-- * Solarized colors

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
