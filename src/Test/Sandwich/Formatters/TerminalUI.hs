{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Test.Sandwich.Formatters.TerminalUI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Control.Monad
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
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
runApp (TerminalUIFormatter {..}) rts = void $ defaultMain theApp initialState
  where
    initialState :: AppState
    initialState = L.list () (Vec.fromList ["a","b","c"]) 1

theApp :: App AppState e ()
theApp = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

type AppState = L.List () String

drawUI :: AppState -> [Widget ()]
drawUI l = [ui]
  where
    ui = vBox [vLimitPercent 10 topBox
              , mainList]

    topBox = vBox [str "This text is at the top.", fill ' ', hBorder]

    mainList = vBox [ hCenter box
                    -- , str " "
                    , hCenter $ str "Press Esc to exit."
                    ]

    box = L.renderList listDrawElement True l

    listDrawElement :: (Show a) => Bool -> a -> Widget ()
    listDrawElement True a = withAttr customAttr $ str (show a)
    listDrawElement False a = str (show a)

appEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
appEvent l (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt l

        ev -> continue =<< L.handleListEvent ev l
appEvent l _ = continue l


theMap :: AttrMap
theMap = attrMap V.defAttr [
  (L.listAttr, V.white `on` V.blue)
  , (L.listSelectedAttr, V.blue `on` V.white)
  , (customAttr, fg V.cyan)
  ]

customAttr :: AttrName
customAttr = L.listSelectedAttr <> "custom"
