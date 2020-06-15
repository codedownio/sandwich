{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.TerminalUI (
  defaultTerminalUIFormatter

  ) where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import qualified Data.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.TreeToList
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree


data TerminalUIFormatter = TerminalUIFormatter {
  showContextManagers :: Bool
  }

defaultTerminalUIFormatter :: TerminalUIFormatter
defaultTerminalUIFormatter = TerminalUIFormatter {
  showContextManagers = True
  }
  
instance Formatter TerminalUIFormatter where
  runFormatter = runApp

runApp :: TerminalUIFormatter -> [RunTree] -> IO ()
runApp (TerminalUIFormatter {..}) rts = do
  rtsFixed <- mapM fixRunTree rts
  let initialState = updateFilteredTree (filterRunTree showContextManagers rtsFixed) $
        AppState {
          _appShowContextManagers = showContextManagers
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

    topBox = vBox [toggleIndicator (app ^. appShowContextManagers) "c" "Hide context managers" "Show context managers"
                  , fill ' '
                  , hBorder]

    toggleIndicator True key onMsg offMsg = keyIndicator key onMsg
    toggleIndicator False key onMsg offMsg = keyIndicator key offMsg

    keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", str msg]
  
    mainList = vBox [ hCenter box
                    -- , str " "
                    , hCenter $ str "Press Esc to exit."
                    ]

    box = padAll 1 $ L.renderList listDrawElement True (app ^. appMainList)

    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement True (MainListElem {..}) = withAttr selectedAttr $ withAttr (chooseAttr status) (str label)
    listDrawElement False (MainListElem {..}) = withAttr (chooseAttr status) (str label)

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
