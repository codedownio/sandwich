{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.TerminalUI (
  defaultTerminalUIFormatter
  , showContextManagers
  , showRunTimes
  ) where

import Brick
import Brick.BChan
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Lens.Micro
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.CrossPlatform
import Test.Sandwich.Formatters.TerminalUI.Draw
import Test.Sandwich.Formatters.TerminalUI.Filter
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.TreeToList
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Shutdown
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
  let initialState = updateFilteredTree (zip (filterRunTree showContextManagers rts) (filterRunTree showContextManagers rtsFixed)) $
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

appEvent :: AppState -> BrickEvent () AppEvent -> EventM () (Next AppState)
appEvent s (AppEvent (RunTreeUpdated newTree)) = continue $ s
  & appRunTree .~ newTree
  & updateFilteredTree (zip (filterRunTree (s ^. appShowContextManagers) (s ^. appRunTreeBase))
                            (filterRunTree (s ^. appShowContextManagers) newTree))

appEvent s x@(VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> halt s
    V.EvKey c [] | c == exitKey-> halt s

    V.EvKey c [] | c == toggleShowContextManagersKey -> do
                     let runTreeFiltered = filterRunTree (not $ s ^. appShowContextManagers) (s ^. appRunTreeBase)
                     let runTreeFilteredFixed = filterRunTree (not $ s ^. appShowContextManagers) (s ^. appRunTree)
                     continue $ s
                       & appShowContextManagers %~ not
                       & updateFilteredTree (zip runTreeFiltered runTreeFilteredFixed)

    V.EvKey c [] | c == toggleShowRunTimesKey -> continue $ s
      & appShowRunTimes %~ not

    V.EvKey c [] | c `elem` toggleKeys -> modifyToggled s not
    V.EvKey c [] | c == V.KLeft -> modifyToggled s (const False)
    V.EvKey c [] | c == V.KRight -> modifyToggled s (const True)

    V.EvKey c [] | c == cancelAllKey -> do
      liftIO $ mapM_ cancelRecursively (s ^. appRunTree)
      continue s
    V.EvKey c [] | c == clearResultsKey -> do
      liftIO $ mapM_ clearRecursively (s ^. appRunTreeBase)
      continue $ s
    V.EvKey c [] | c == openSelectedFolderInFileExplorer -> do
      case L.listSelectedElement (s ^. appMainList) of
        Just (_i, MainListElem {folderPath=(Just path)}) ->
          liftIO $ openFileExplorerFolderPortable path
        _ -> return () -- Shouldn't happen
      continue $ s

    -- V.EvKey (V.KChar c) [] | c == runAgainKey -> do
    --   liftIO $
    --   continue s

    ev -> handleEventLensed s appMainList L.handleListEvent ev >>= continue
appEvent s _ = continue s

modifyToggled s f = case L.listSelectedElement (s ^. appMainList) of
  Nothing -> continue s
  Just (i, MainListElem {..}) -> do
    liftIO $ atomically $ modifyTVar (runTreeToggled node) f
    continue s


updateFilteredTree :: [(RunTree, RunTreeFixed)] -> AppState -> AppState
updateFilteredTree pairs s = s
  & appRunTreeFiltered .~ fmap snd pairs
  & appMainList %~ L.listReplace (treeToVector pairs)
                                 (L.listSelected $ s ^. appMainList)

-- * Clearing

clearRecursively :: RunTree -> IO ()
clearRecursively (RunTreeGroup {..}) = do
  forM_ runTreeChildren clearRecursively
  atomically $ writeTVar runTreeStatus NotStarted
  atomically $ writeTVar runTreeLogs mempty
clearRecursively (RunTreeSingle {..}) = do
  atomically $ writeTVar runTreeStatus NotStarted
  atomically $ writeTVar runTreeLogs mempty
