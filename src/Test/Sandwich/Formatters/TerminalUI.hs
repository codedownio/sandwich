{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Formatters.TerminalUI (
  defaultTerminalUIFormatter

  ) where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Brick.Widgets.ProgressBar
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Clock
import qualified Graphics.Vty as V
import Lens.Micro
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Count
import Test.Sandwich.Formatters.TerminalUI.Filter
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.TreeToList
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Formatters.TerminalUI.Util
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree


data TerminalUIFormatter = TerminalUIFormatter {
  showContextManagers :: Bool
  , showRunTimes :: Bool
  }

defaultTerminalUIFormatter :: TerminalUIFormatter
defaultTerminalUIFormatter = TerminalUIFormatter {
  showContextManagers = True
  , showRunTimes = True
  }
  
instance Formatter TerminalUIFormatter where
  runFormatter = runApp

runApp :: TerminalUIFormatter -> [RunTree] -> IO ()
runApp (TerminalUIFormatter {..}) rts = do
  rtsFixed <- atomically $ mapM fixRunTree rts
  let initialState = updateFilteredTree (filterRunTree showContextManagers rtsFixed) $
        AppState {
          _appRunTreeBase = rts
          , _appRunTree = rtsFixed
          , _appRunTreeFiltered = []
          , _appMainList = L.list () mempty 1

          , _appShowContextManagers = showContextManagers
          , _appShowRunTimes = showRunTimes
        }

  eventChan <- newBChan 10

  currentFixedTree <- newTVarIO rtsFixed
  async $ forever $ do
    newFixedTree <- atomically $ do
      currentFixed <- readTVar currentFixedTree
      newFixed <- mapM fixRunTree rts
      when (newFixed == currentFixed) retry
      writeTVar currentFixedTree newFixed
      return newFixed
    writeBChan eventChan (RunTreeUpdated newFixedTree)
  
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just eventChan) app initialState

app :: App AppState AppEvent ()
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const mainAttrMap
  }

drawUI :: AppState -> [Widget ()]
drawUI app = [ui]
  where
    ui = vBox [vLimitPercent 10 topBox
              , mainList]

    keybindingBox = vBox

    topBox = vBox [hBox [padRight (Pad 3) $ hLimitPercent 33 (keybindingBox [toggleIndicator (app ^. appShowContextManagers) [toggleShowContextManagersKey] "Hide context managers" "Show context managers"
                                                                            , toggleIndicator (app ^. appShowRunTimes) [toggleShowRunTimesKey] "Hide run times" "Show run times"])
                        , vBorder
                        , padLeftRight 3 $ hLimitPercent 33 (keybindingBox [keyIndicator [cancelAllKey] "Cancel all"
                                                                           , keyIndicator [cancelSelectedKey] "Cancel selected"
                                                                           , keyIndicator [clearResultsKey] "Clear results"
                                                                           , keyIndicator [runAgainKey] "Run again"])
                        , vBorder
                        , padLeftRight 3 $ hLimitPercent 33 (keybindingBox [keyIndicator "q" "Exit"])]
                  , hBorderWithLabel $ padLeftRight 1 $ hBox (L.intercalate [str ", "] countWidgets <> [str [i| of #{totalNumTests}|]])]

    countWidgets =
      (if totalSucceededTests > 0 then [[withAttr successAttr $ str $ show totalSucceededTests, str " succeeded"]] else mempty)
      <> (if totalFailedTests > 0 then [[withAttr failureAttr $ str $ show totalFailedTests, str " failed"]] else mempty)
      <> (if totalPendingTests > 0 then [[withAttr pendingAttr $ str $ show totalPendingTests, str " pending"]] else mempty)
      <> (if totalRunningTests > 0 then [[withAttr runningAttr $ str $ show totalRunningTests, str " running"]] else mempty)

    totalNumTests = countWhere isItBlock (app ^. appRunTree)
    totalSucceededTests = countWhere isSuccessItBlock (app ^. appRunTree)
    totalPendingTests = countWhere isPendingItBlock (app ^. appRunTree)
    totalFailedTests = countWhere isFailedItBlock (app ^. appRunTree)
    totalRunningTests = countWhere isRunningItBlock (app ^. appRunTree)
    totalDoneTests = countWhere isDoneItBlock (app ^. appRunTree)

    toggleIndicator True key onMsg offMsg = keyIndicator key onMsg
    toggleIndicator False key onMsg offMsg = keyIndicator key offMsg

    keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", str msg]
  
    mainList = vBox [hCenter box
                    , progressBar Nothing (fromIntegral totalDoneTests / fromIntegral totalNumTests)]

    box = padAll 1 $ L.renderList listDrawElement True (app ^. appMainList)

    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement True elem = withAttr selectedAttr $ renderElem elem
    listDrawElement False elem = renderElem elem

    -- renderElem elem@(MainListElem {..}) = padLeft (Pad (4 * depth)) $ vBox [renderLine elem, border $ str $ show status]
    renderElem elem@(MainListElem {..}) = padLeft (Pad (4 * depth)) $ renderLine elem

    renderLine (MainListElem {..}) = hBox $ catMaybes [
      Just $ withAttr toggleMarkerAttr $ str "[+] "
      , Just $ padRight Max $ withAttr (chooseAttr status) (str label)
      , if not (app ^. appShowRunTimes) then Nothing else case status of
          Running {..} -> Just $ str $ "    " <> show statusStartTime
          Done {..} -> Just $ str $ "    " <> formatNominalDiffTime (diffUTCTime statusEndTime statusStartTime)
          _ -> Nothing
      ]

appEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
appEvent s (AppEvent (RunTreeUpdated newTree)) = continue $ s
  & appRunTree .~ newTree
  & updateFilteredTree (filterRunTree (s ^. appShowContextManagers) newTree)

appEvent s x@(VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt s
    V.EvKey (V.KChar c) [] | c == exitKey-> halt s

    V.EvKey (V.KChar c) [] | c == toggleShowContextManagersKey -> continue $
      let runTreeFiltered = filterRunTree (not $ s ^. appShowContextManagers) (s ^. appRunTree) in s
      & appShowContextManagers %~ not
      & updateFilteredTree runTreeFiltered

    V.EvKey (V.KChar c) [] | c == toggleShowRunTimesKey -> continue $ s
      & appShowRunTimes %~ not

    V.EvKey (V.KChar c) [] | c == cancelAllKey -> do
      liftIO $ mapM_ cancelRecursively (s ^. appRunTree)
      continue s
    V.EvKey (V.KChar c) [] | c == clearResultsKey -> do
                               liftIO $ mapM_ clearRecursively (s ^. appRunTreeBase)
                               continue $ s

    -- V.EvKey (V.KChar c) [] | c == runAgainKey -> do
    --   liftIO $
    --   continue s

    ev -> handleEventLensed s appMainList L.handleListEvent ev >>= continue
appEvent s _ = continue s

updateFilteredTree :: [RunTreeFixed] -> AppState -> AppState
updateFilteredTree runTreeFiltered s = s
  & appRunTreeFiltered .~ runTreeFiltered
  & appMainList %~ L.listReplace (treeToVector runTreeFiltered)
                                 (L.listSelected $ s ^. appMainList)

-- * Filter tree


-- * Cancelling

cancelRecursively :: RunTreeFixed -> IO ()
cancelRecursively (RunTreeGroup {..}) = do
  forM_ runTreeChildren cancelRecursively
  cancel runTreeAsync
cancelRecursively (RunTreeSingle {..}) =
  cancel runTreeAsync

-- * Clearing

clearRecursively :: RunTree -> IO ()
clearRecursively (RunTreeGroup {..}) = do
  forM_ runTreeChildren clearRecursively
  atomically $ writeTVar runTreeStatus NotStarted
  atomically $ writeTVar runTreeLogs mempty
clearRecursively (RunTreeSingle {..}) = do
  atomically $ writeTVar runTreeStatus NotStarted
  atomically $ writeTVar runTreeLogs mempty
