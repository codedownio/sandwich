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

  , (toggleMarkerAttr, fg (grayAt 50))
  
  , (hotkeyAttr, fg V.blue)
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

hotkeyAttr :: AttrName
hotkeyAttr = "hotkey"

chooseAttr :: Status -> AttrName
chooseAttr NotStarted = notStartedAttr
chooseAttr (Running {}) = runningAttr
chooseAttr (Done _ _ (Pending {})) = pendingAttr
chooseAttr (Done _ _ (Success {})) = successAttr
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
