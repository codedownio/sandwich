{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

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
import GHC.Stack
import qualified Graphics.Vty as V
import Lens.Micro
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Draw.ColorProgressBar
import Test.Sandwich.Formatters.TerminalUI.Draw.ToBrickWidget
import Test.Sandwich.Formatters.TerminalUI.Draw.TopBox
import Test.Sandwich.Formatters.TerminalUI.Draw.Util
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree


drawUI :: AppState -> [Widget ClickableName]
drawUI app = [ui]
  where
    ui = vBox [
      topBox app
      , borderWithCounts app
      , mainList app
      , clickable ColorBar $ bottomProgressBarColored app
      ]

mainList app = hCenter $ padAll 1 $ L.renderListWithIndex listDrawElement True (app ^. appMainList)
  where
    listDrawElement ix isSelected x@(MainListElem {..}) = clickable (ListRow ix) $ padLeft (Pad (4 * depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      , do
          guard toggled
          let infoWidgets = getInfoWidgets x
          guard (not $ L.null infoWidgets)
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{ident}|]) 33 $
              vBox infoWidgets
      ]

    renderLine _isSelected (MainListElem {..}) = hBox $ catMaybes [
      Just $ withAttr openMarkerAttr $ str (if open then "[-] " else "[+] ")
      , Just $ withAttr (chooseAttr status) (str label)
      , if not (app ^. appShowFileLocations) then Nothing else
          case runTreeLoc node of
            Nothing -> Nothing
            Just loc ->
              Just $ hBox [str " ["
                          , withAttr logFilenameAttr $ str $ srcLocFile loc
                          , str ":"
                          , withAttr logLineAttr $ str $ show $ srcLocStartLine loc
                          , str "]"]
      , if not (app ^. appShowVisibilityThresholds) then Nothing else
          Just $ hBox [str " ["
                      , withAttr visibilityThresholdIndicatorMutedAttr $ str "V="
                      , withAttr visibilityThresholdIndicatorAttr $ str $ show visibilityLevel
                      , str "]"]
      , Just $ padRight Max $ withAttr toggleMarkerAttr $ str (if toggled then " [-]" else " [+]")
      , if not (app ^. appShowRunTimes) then Nothing else case status of
          Running {..} -> Just $ str $ show statusStartTime
          Done {..} -> Just $ raw $ V.string attr $ formatNominalDiffTime (diffUTCTime statusEndTime statusStartTime)
            where totalElapsed = realToFrac (max (app ^. appTimeSinceStart) (diffUTCTime statusEndTime (app ^. appStartTime)))
                  duration = realToFrac (diffUTCTime statusEndTime statusStartTime)
                  intensity :: Double = logBase (totalElapsed + 1) (duration + 1)
                  minGray :: Int = 50
                  maxGray :: Int = 255
                  level :: Int = min maxGray $ max minGray $ round (fromIntegral minGray + (intensity * (fromIntegral (maxGray - minGray))))
                  attr = V.Attr {
                    V.attrStyle = V.Default
                    , V.attrForeColor = V.SetTo (grayAt level)
                    , V.attrBackColor = V.Default
                    , V.attrURL = V.Default
                    }
          _ -> Nothing
      ]

    getInfoWidgets mle@(MainListElem {..}) = catMaybes [Just $ toBrickWidget (app ^. appCustomExceptionFormatters) status, callStackWidget mle, logWidget mle]

    callStackWidget (MainListElem {..}) = do
      cs <- getCallStackFromStatus status
      return $ borderWithLabel (padLeftRight 1 $ str "Callstack") $ toBrickWidget (app ^. appCustomExceptionFormatters) cs

    logWidget (MainListElem {..}) = do
      let filteredLogs = case app ^. appLogLevel of
            Nothing -> mempty
            Just logLevel -> Seq.filter (\x -> logEntryLevel x >= logLevel) logs
      guard (not $ Seq.null filteredLogs)
      return $ borderWithLabel (padLeftRight 1 $ str "Logs") $ vBox $
        toList $ fmap logEntryWidget filteredLogs

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


borderWithCounts app = hBorderWithLabel $ padLeftRight 1 $ hBox (L.intercalate [str ", "] countWidgets <> [str [i| of |]
                                                                                                          , withAttr totalAttr $ str $ show totalNumTests
                                                                                                          , str [i| in |]
                                                                                                          , withAttr timeAttr $ str $ formatNominalDiffTime (app ^. appTimeSinceStart)])
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
