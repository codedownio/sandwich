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
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Clock
import qualified Graphics.Vty as V
import Lens.Micro
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.TreeToList
import Test.Sandwich.Formatters.TerminalUI.Types
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
  rtsFixed <- mapM fixRunTree rts
  let initialState = updateFilteredTree (filterRunTree showContextManagers rtsFixed) $
        AppState {
          _appShowContextManagers = showContextManagers
          , _appShowRunTimes = showRunTimes
          , _appRunTree = rtsFixed
          , _appRunTreeFiltered = []
          , _appMainList = L.list () mempty 1
        }

  eventChan <- newBChan 10

  async $ forever $ do
    rtsFixed <- mapM fixRunTree rts
    writeBChan eventChan (RunTreeUpdated rtsFixed)
    threadDelay 100000
  
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

    topBox = vBox [hBox [hLimitPercent 33 (vBox [toggleIndicator (app ^. appShowContextManagers) "c" "Hide context managers" "Show context managers"
                                                , toggleIndicator (app ^. appShowRunTimes) "t" "Hide run times" "Show run times"])
                        , hLimitPercent 33 (vBox [keyIndicator "C" "Clear results"])
                        , hLimitPercent 33 (vBox [keyIndicator "q" "Exit"])]
                  , fill ' '
                  , hBorderWithLabel $ str [i|  #{totalRunningTests} running, #{totalDoneTests} done of #{totalNumTests}  |]]

    totalNumTests = countWhere isItBlock (app ^. appRunTree)
    totalRunningTests = countWhere isRunningItBlock (app ^. appRunTree)
    totalDoneTests = countWhere isDoneItBlock (app ^. appRunTree)

    toggleIndicator True key onMsg offMsg = keyIndicator key onMsg
    toggleIndicator False key onMsg offMsg = keyIndicator key offMsg

    keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", str msg]
  
    mainList = vBox [ hCenter box
                    -- , str " "
                    , progressBar Nothing (fromIntegral totalDoneTests / fromIntegral totalNumTests)
                    ]

    box = padAll 1 $ L.renderList listDrawElement True (app ^. appMainList)

    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement True elem = withAttr selectedAttr $ renderElem elem
    listDrawElement False elem = renderElem elem

    renderElem (MainListElem {..}) = hBox $ catMaybes [
      Just $ withAttr (chooseAttr status) (str label)
      , case status of
          Running startTime _ -> Just $ str $ "    " <> show startTime
          Done startTime endTime _ _ -> Just $ str $ "    " <> show (diffUTCTime endTime startTime)
          _ -> Nothing
      ]

appEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
appEvent s (AppEvent (RunTreeUpdated newTree)) = continue $ s
  & appRunTree .~ newTree
  & updateFilteredTree (filterRunTree (s ^. appShowContextManagers) newTree)

appEvent s x@(VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt s
    V.EvKey (V.KChar 'q') [] -> halt s

    V.EvKey (V.KChar 'c') [] -> continue $
      let runTreeFiltered = filterRunTree (not $ s ^. appShowContextManagers) (s ^. appRunTree) in s
      & appShowContextManagers %~ not
      & updateFilteredTree runTreeFiltered

    ev -> handleEventLensed s appMainList L.handleListEvent ev >>= continue

appEvent s _ = continue s

updateFilteredTree :: [RunTreeFixed] -> AppState -> AppState
updateFilteredTree runTreeFiltered s = s
  & appRunTreeFiltered .~ runTreeFiltered
  & appMainList %~ L.listReplace (treeToVector runTreeFiltered)
                                 (L.listSelected $ s ^. appMainList)

-- * Filter tree

filterRunTree :: Bool -> [RunTreeFixed] -> [RunTreeFixed]
filterRunTree showContextManagers rtsFixed = rtsFixed
  & if showContextManagers then id else filterContextManagers

filterContextManagers :: [RunTreeFixed] -> [RunTreeFixed]
filterContextManagers = mconcat . fmap filterContextManagersSingle

filterContextManagersSingle :: RunTreeFixed -> [RunTreeFixed]
filterContextManagersSingle rt@(RunTreeGroup {runTreeIsContextManager=False, ..}) = [rt { runTreeChildren = filterContextManagers runTreeChildren }]
filterContextManagersSingle (RunTreeGroup {runTreeIsContextManager=True, ..}) = filterContextManagers runTreeChildren
filterContextManagersSingle rt@(RunTreeSingle {}) = [rt]

-- * Counting

countWhere :: (RunTreeFixed -> Bool) -> [RunTreeFixed] -> Int
countWhere p rts = sum $ fmap (countWhere' p) rts

countWhere' :: (RunTreeFixed -> Bool) -> RunTreeFixed -> Int
countWhere' p rt@(RunTreeGroup {..}) =
  (if p rt then 1 else 0) + countWhere p runTreeChildren
countWhere' picate rt@(RunTreeSingle {..}) = if picate rt then 1 else 0

isItBlock (RunTreeSingle {}) = True
isItBlock _ = False

isRunningItBlock (RunTreeSingle {runTreeStatus=(Running {})}) = True
isRunningItBlock _ = False

isDoneItBlock (RunTreeSingle {runTreeStatus=(Done {})}) = True
isDoneItBlock _ = False
