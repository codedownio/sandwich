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
import Test.Sandwich.Formatters.TerminalUI.AttrMap
-- import Lens.Micro ((^.))
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
        showContextManagers = showContextManagers
        , runTree = rtsFixed
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

data AppState = AppState {
  showContextManagers :: Bool
  , runTree :: [RunTreeFixed]
  }

drawUI :: AppState -> [Widget ()]
drawUI (AppState {..}) = [ui]
  where
    ui = vBox [vLimitPercent 10 topBox
              , mainList]

    topBox = vBox [str "This text is at the top.", fill ' ', hBorder]

    mainList = vBox [ hCenter box
                    -- , str " "
                    , hCenter $ str "Press Esc to exit."
                    ]

    box = padAll 1 $ L.renderList listDrawElement True (L.list () (runTreesToList runTree) 1)

    listDrawElement :: Bool -> MainListElem -> Widget ()
    listDrawElement True (MainListElem {..}) = withAttr selectedAttr $ withAttr (chooseAttr status) (str label)
    listDrawElement False (MainListElem {..}) = withAttr (chooseAttr status) (str label)


appEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
appEvent s (AppEvent (RunTreeUpdated newTree)) = continue $ s { runTree = newTree }
appEvent s (VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt s
    ev -> continue s
appEvent s _ = continue s

-- * Events

data AppEvent = RunTreeUpdated [RunTreeFixed]

-- * Main list

data MainListElem = MainListElem {
  label :: String
  , folded :: Bool
  , status :: Status
  } deriving Show

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
