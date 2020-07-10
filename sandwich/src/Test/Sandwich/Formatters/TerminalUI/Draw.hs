{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Draw where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Control.Monad
import Control.Monad.Logger
import Data.Foldable
import qualified Data.List as L
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.String.Interpolate
import qualified Data.Text.Encoding as E
import Data.Time.Clock
import Lens.Micro
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Draw.ColorProgressBar
import Test.Sandwich.Formatters.TerminalUI.Draw.ToBrickWidget
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Formatters.TerminalUI.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


drawUI :: AppState -> [Widget ()]
drawUI app = [ui]
  where
    ui = vBox [
      topBox app
      , borderWithCounts app
      , mainList app
      , bottomProgressBarColored app
      ]

mainList app = hCenter $ padAll 1 $ L.renderList listDrawElement True (app ^. appMainList)
  where
    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement isSelected x@(MainListElem {..}) = padLeft (Pad (4 * depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      , do
          guard toggled
          let infoWidgets = getInfoWidgets x
          guard (not $ L.null infoWidgets)
          return $ padLeft (Pad 4) (vBox infoWidgets)
      ]

    renderLine isSelected (MainListElem {..}) = hBox $ catMaybes [
      Just $ withAttr toggleMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , Just $ padRight Max $ withAttr (chooseAttr status) (str label)
      , if not (app ^. appShowRunTimes) then Nothing else case status of
          Running {..} -> Just $ str $ show statusStartTime
          Done {..} -> Just $ str $ formatNominalDiffTime (diffUTCTime statusEndTime statusStartTime)
          _ -> Nothing
      ]

    getInfoWidgets (MainListElem {..}) = catMaybes [
      Just $ toBrickWidget status
      , do
          cs <- getCallStackFromStatus status
          return $ borderWithLabel (padLeftRight 1 $ str "Callstack") $ toBrickWidget cs
      , do
          guard (not $ Seq.null logs)
          return $ borderWithLabel (padLeftRight 1 $ str "Logs") $ vBox (toList $ fmap logEntryWidget logs)
      ]

    getResultTitle (NotStarted {}) = str "Not started"
    getResultTitle (Running {}) = withAttr runningAttr $ str "Running"
    getResultTitle (Done {statusResult=(Success)}) = withAttr successAttr $ str "Success"
    getResultTitle (Done {statusResult=(Failure (Pending {}))}) = withAttr pendingAttr $ str "Pending"
    getResultTitle (Done {statusResult=(Failure _)}) = withAttr failureAttr $ str "Failure"

    logEntryWidget (LogEntry {..}) = hBox [
      withAttr logTimestampAttr $ str (show logEntryTime)
      , str " "
      , logLevelWidget logEntryLevel
      , str " "
      , logLocWidget logEntryLoc
      , str " "
      , txtWrap (E.decodeUtf8 $ fromLogStr logEntryStr)
      ]

    logLocWidget (Loc {loc_start=(line, ch), ..}) = hBox [
      str "["
      , withAttr logFilenameAttr $ str loc_filename
      , str ":"
      , withAttr logLineAttr $ str (show line)
      , str ":"
      , withAttr logChAttr $ str (show ch)
      , str "]"
      ]

    logLevelWidget LevelDebug = withAttr debugAttr $ str "(DEBUG)"
    logLevelWidget LevelInfo = withAttr infoAttr $ str "(INFO)"
    logLevelWidget LevelWarn = withAttr infoAttr $ str "(WARN)"
    logLevelWidget LevelError = withAttr infoAttr $ str "(ERROR)"
    logLevelWidget (LevelOther x) = withAttr infoAttr $ str [i|#{x}|]

topBox app = hBox [columnPadding $ hLimitPercent 33 settingsColumn
                  , columnPadding $ hLimitPercent 33 actionsColumn
                  , columnPadding $ hLimitPercent 33 otherActionsColumn]
  where
    columnPadding = padLeft (Pad 1) . padRight (Pad 3) -- . padTop (Pad 1)

    settingsColumn = keybindingBox [keyIndicator (unKChar nextKey : "/↑") "Next"
                                   , keyIndicator (unKChar previousKey : "/↓") "Previous"
                                   , keyIndicator (unKChar nextFailureKey : "/↑") "Next failure"
                                   , keyIndicator (unKChar previousFailureKey : "/↑") "Previous failure"
                                   , keyIndicatorHasSelected (showKeys toggleKeys) "Toggle selected"]

    actionsColumn = keybindingBox [keyIndicator (showKey cancelAllKey) "Cancel all"
                                  , keyIndicatorHasSelected (showKey cancelSelectedKey) "Cancel selected"
                                  , keyIndicator (showKey clearResultsKey) "Clear results"
                                  , keyIndicator (showKey runAgainKey) "Run again"
                                  , keyIndicatorHasSelectedAndFolder (showKey openSelectedFolderInFileExplorer) "Open in file explorer"
                                  ]

    otherActionsColumn = keybindingBox [toggleIndicator (app ^. appShowContextManagers) (showKey toggleShowContextManagersKey) "Hide context managers" "Show context managers"
                                       , toggleIndicator (app ^. appShowRunTimes) (showKey toggleShowRunTimesKey) "Hide run times" "Show run times"
                                       , keyIndicator "q" "Exit"]

    keybindingBox = vBox

    toggleIndicator True key onMsg _ = keyIndicator key onMsg
    toggleIndicator False key _ offMsg = keyIndicator key offMsg

    keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]

    keyIndicatorHasSelected = keyIndicatorContextual (\s -> isJust $ L.listSelectedElement (s ^. appMainList))

    keyIndicatorHasSelectedAndFolder = keyIndicatorContextual $ \s -> case L.listSelectedElement (s ^. appMainList) of
      Just (_, MainListElem {folderPath=(Just _)}) -> True
      _ -> False

    keyIndicatorContextual p key msg = case p app of
      True -> hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]
      False -> hBox [str "[", withAttr disabledHotkeyAttr $ str key, str "] ", withAttr disabledHotkeyMessageAttr $ str msg]


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
    totalNotStartedTests = countWhere isNotStartedItBlock (app ^. appRunTree)
