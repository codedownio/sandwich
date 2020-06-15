{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
  let initialState = AppState {
        _appShowContextManagers = showContextManagers
        , _appRunTree = rtsFixed
        , _appMainList = treeToList rtsFixed
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

    mainList = vBox [ hCenter box
                    -- , str " "
                    , hCenter $ str "Press Esc to exit."
                    ]

    box = padAll 1 $ L.renderList listDrawElement True (app ^. appMainList)

    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement True (MainListElem {..}) = withAttr selectedAttr $ withAttr (chooseAttr status) (str label)
    listDrawElement False (MainListElem {..}) = withAttr (chooseAttr status) (str label)

toggleIndicator True key onMsg offMsg = keyIndicator key onMsg
toggleIndicator False key onMsg offMsg = keyIndicator key offMsg

keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", str msg]

appEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
appEvent s (AppEvent (RunTreeUpdated newTree)) = continue $ s
  & appRunTree .~ newTree
  & appMainList %~ L.listReplace (runTreesToList newTree) (L.listSelected $ s ^. appMainList)

appEvent s x@(VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt s
    V.EvKey (V.KChar 'q') [] -> halt s

    V.EvKey (V.KChar 'c') [] -> continue $ s & appShowContextManagers %~ not

    ev -> handleEventLensed s appMainList L.handleListEvent ev >>= continue

appEvent s _ = continue s

-- * Main list

treeToList :: [RunTreeFixed] -> L.GenericList () Vec.Vector MainListElem
treeToList runTree = L.list () (runTreesToList runTree) 1

runTreesToList :: [RunTreeFixed] -> Vec.Vector MainListElem
runTreesToList = runTreesToList' 0

runTreesToList' :: Int -> [RunTreeFixed] -> Vec.Vector MainListElem
runTreesToList' indent rts = mconcat $ fmap (runTreeToList' indent) rts

runTreeToList' :: Int -> RunTreeFixed -> Vec.Vector MainListElem
runTreeToList' indent (RunTreeGroup {..}) = elem `Vec.cons` (runTreesToList' (indent + 1) runTreeChildren)
  where elem = MainListElem {
          label = (L.replicate (indent * 4) ' ') <> runTreeLabel
          , folded = False
          , status = runTreeStatus
          }
runTreeToList' indent (RunTreeSingle {..}) = Vec.singleton elem
  where elem = MainListElem {
          label = (L.replicate (indent * 4) ' ') <> runTreeLabel
          , folded = False
          , status = runTreeStatus
          }
