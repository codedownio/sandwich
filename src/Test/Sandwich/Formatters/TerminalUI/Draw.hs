{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Draw where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Brick.Widgets.ProgressBar
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Clock
import GHC.Stack
import Lens.Micro
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Count
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Formatters.TerminalUI.Util
import Test.Sandwich.Types.RunTree


drawUI :: AppState -> [Widget ()]
drawUI app = [ui]
  where
    ui = vBox [vLimitPercent 10 (topBox app)
              , borderWithCounts app
              , mainList app
              , bottomProgressBar app]

mainList app = hCenter $ padAll 1 $ L.renderList listDrawElement True (app ^. appMainList)
  where
    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement isSelected x@(MainListElem {..}) = padLeft (Pad (4 * depth)) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      , do
          guard toggled
          let infoWidgets = getInfoWidgets x
          guard (not $ L.null infoWidgets)
          return $ padLeft (Pad 4) (vBox infoWidgets)
      ]

    renderLine isSelected (MainListElem {..}) = (if isSelected then border else id) $ hBox $ catMaybes [
      Just $ withAttr toggleMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , Just $ padRight Max $ withAttr (chooseAttr status) (str label)
      , if not (app ^. appShowRunTimes) then Nothing else case status of
          Running {..} -> Just $ str $ show statusStartTime
          Done {..} -> Just $ str $ formatNominalDiffTime (diffUTCTime statusEndTime statusStartTime)
          _ -> Nothing
      ]

    getInfoWidgets (MainListElem {..}) = catMaybes [
      Just $ strWrap $ show status
      , do
          cs <- getCallStackFromStatus status
          return $ border $ strWrap $ prettyCallStack cs
      , do
          guard (logs /= mempty)
          return $ strWrap $ show logs
      ]

topBox app = vBox [hBox [padRight (Pad 3) $ hLimitPercent 33 settingsColumn
                        , vBorder
                        , padLeftRight 3 $ hLimitPercent 33 actionsColumn
                        , vBorder
                        , padLeftRight 3 $ hLimitPercent 33 otherActionsColumn]]
  where
    settingsColumn = keybindingBox [toggleIndicator (app ^. appShowContextManagers) (showKey toggleShowContextManagersKey) "Hide context managers" "Show context managers"
                                   , toggleIndicator (app ^. appShowRunTimes) (showKey toggleShowRunTimesKey) "Hide run times" "Show run times"
                                   , keyIndicator (showKeys toggleKeys) "Toggle selected"]

    actionsColumn = keybindingBox [keyIndicator (showKey cancelAllKey) "Cancel all"
                                  , keyIndicator (showKey cancelSelectedKey) "Cancel selected"
                                  , keyIndicator (showKey clearResultsKey) "Clear results"
                                  , keyIndicator (showKey runAgainKey) "Run again"]

    otherActionsColumn = keybindingBox [keyIndicator "q" "Exit"]

    keybindingBox = vBox

    toggleIndicator True key onMsg offMsg = keyIndicator key onMsg
    toggleIndicator False key onMsg offMsg = keyIndicator key offMsg

    keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", str msg]


borderWithCounts app = hBorderWithLabel $ padLeftRight 1 $ hBox (L.intercalate [str ", "] countWidgets <> [str [i| of #{totalNumTests}|]])
  where
    countWidgets =
      (if totalSucceededTests > 0 then [[withAttr successAttr $ str $ show totalSucceededTests, str " succeeded"]] else mempty)
      <> (if totalFailedTests > 0 then [[withAttr failureAttr $ str $ show totalFailedTests, str " failed"]] else mempty)
      <> (if totalPendingTests > 0 then [[withAttr pendingAttr $ str $ show totalPendingTests, str " pending"]] else mempty)
      <> (if totalRunningTests > 0 then [[withAttr runningAttr $ str $ show totalRunningTests, str " running"]] else mempty)
      <> (if totalNotStartedTests > 0 then [[str $ show totalNotStartedTests, str " not started"]] else mempty)

    totalNumTests = countWhere isItBlock (app ^. appRunTree)
    totalSucceededTests = countWhere isSuccessItBlock (app ^. appRunTree)
    totalPendingTests = countWhere isPendingItBlock (app ^. appRunTree)
    totalFailedTests = countWhere isFailedItBlock (app ^. appRunTree)
    totalRunningTests = countWhere isRunningItBlock (app ^. appRunTree)
    totalDoneTests = countWhere isDoneItBlock (app ^. appRunTree)
    totalNotStartedTests = countWhere isNotStartedItBlock (app ^. appRunTree)

bottomProgressBar app = progressBar Nothing (fromIntegral totalDoneTests / fromIntegral totalNumTests)
  where
    totalNumTests = countWhere isItBlock (app ^. appRunTree)
    totalDoneTests = countWhere isDoneItBlock (app ^. appRunTree)