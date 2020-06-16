{-# LANGUAGE OverloadedStrings #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.AttrMap where

import Brick
import Brick.Widgets.List
import Brick.Widgets.ProgressBar
import qualified Graphics.Vty as V
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.RunTree


mainAttrMap :: AttrMap
mainAttrMap = attrMap V.defAttr [
  -- (listAttr, V.white `on` V.blue)
   -- (listSelectedAttr, V.blue `on` V.white)
  (listSelectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))

  -- , (selectedAttr, fg V.cyan)

  -- , (notStartedAttr, fg V.)
  , (runningAttr, fg V.blue)
  , (pendingAttr, fg V.yellow)
  , (successAttr, fg V.green)
  , (failureAttr, fg V.red)

  , (progressCompleteAttr, bg (V.Color240 235))
  , (progressIncompleteAttr, bg (V.Color240 225))

  , (toggleMarkerAttr, fg (grayAt 50))
  
  , (hotkeyAttr, fg V.blue)
  ]

grayAt level = V.Color240 $ V.rgbColorToColor240 level level level

selectedAttr :: AttrName
selectedAttr = listSelectedAttr <> "custom"

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
