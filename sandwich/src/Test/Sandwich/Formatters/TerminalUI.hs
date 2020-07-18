{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.TerminalUI (
  defaultTerminalUIFormatter
  , terminalUIVisibilityThreshold
  , terminalUIShowRunTimes
  , terminalUILogLevel
  , terminalUIInitialFolding
  , InitialFolding(..)
  ) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Time
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.CrossPlatform
import Test.Sandwich.Formatters.TerminalUI.Draw
import Test.Sandwich.Formatters.TerminalUI.Filter
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.TreeToList
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Interpreters.StartTree
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Util


instance Formatter TerminalUIFormatter where
  runFormatter = runApp

runApp :: TerminalUIFormatter -> [RunNode BaseContext] -> BaseContext -> IO ()
runApp (TerminalUIFormatter {..}) rts baseContext = do
  startTime <- getCurrentTime

  let filteredTree = filterRunTree terminalUIVisibilityThreshold rts
  liftIO $ setInitialFolding terminalUIInitialFolding filteredTree

  rtsFixed <- atomically $ mapM fixRunTree rts
  let initialState = updateFilteredTree $
        AppState {
          _appRunTreeBase = rts
          , _appRunTree = rtsFixed
          , _appRunTreeFiltered = []
          , _appMainList = list MainList mempty 1
          , _appBaseContext = baseContext

          , _appStartTime = startTime
          , _appTimeSinceStart = 0

          , _appVisibilityThresholdSteps = L.sort $ L.nub $ fmap runTreeVisibilityLevel $ concatMap getCommons rts
          , _appVisibilityThreshold = terminalUIVisibilityThreshold

          , _appLogLevel = terminalUILogLevel
          , _appShowRunTimes = terminalUIShowRunTimes
        }

  eventChan <- newBChan 10

  currentFixedTree <- newTVarIO rtsFixed
  eventAsync <- async $ forever $ do
    newFixedTree <- atomically $ do
      currentFixed <- readTVar currentFixedTree
      newFixed <- mapM fixRunTree rts
      when (fmap getCommons newFixed == fmap getCommons currentFixed) retry
      writeTVar currentFixedTree newFixed
      return newFixed
    writeBChan eventChan (RunTreeUpdated newFixedTree)
    threadDelay 100000 -- Sleep 100ms

  let buildVty = do
        v <- V.mkVty V.defaultConfig
        let output = V.outputIface v
        when (V.supportsMode output V.Mouse) $
          liftIO $ V.setMode output V.Mouse True
        return v
  initialVty <- buildVty
  flip onException (cancel eventAsync) $
    void $ customMain initialVty buildVty (Just eventChan) app initialState

app :: App AppState AppEvent ClickableName
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const mainAttrMap
  }

appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName (Next AppState)
appEvent s (AppEvent (RunTreeUpdated newTree)) = do
  now <- liftIO getCurrentTime
  continue $ s
    & appRunTree .~ newTree
    & appTimeSinceStart .~ (diffUTCTime now (s ^. appStartTime))
    & updateFilteredTree

appEvent s (MouseDown ColorBar _ _ (B.Location (x, _))) = do
  lookupExtent ColorBar >>= \case
    Nothing -> continue s
    Just (Extent {extentSize=(w, _), extentUpperLeft=(B.Location (l, _))}) -> do
      let percent :: Double = (fromIntegral (x - l)) / (fromIntegral w)
      let allCommons = concatMap getCommons $ s ^. appRunTreeFiltered
      let index = max 0 $ min (length allCommons - 1) $ round $ percent * (fromIntegral $ (length allCommons - 1))
      -- A subsequent RunTreeUpdated will pick up the new open nodes
      liftIO $ openIndices (s ^. appRunTreeBase) (runTreeAncestors $ allCommons !! index)
      continue $ s
        & appMainList %~ (listMoveTo index)
        & updateFilteredTree

appEvent s (MouseDown (ListRow i) _ _ _) =
  continue (s & appMainList %~ (listMoveTo i))
appEvent s x@(VtyEvent e) =
  case e of
    -- Column 1
    V.EvKey c [] | c == nextKey -> continue (s & appMainList %~ (listMoveBy 1))
    V.EvKey c [] | c == previousKey -> continue (s & appMainList %~ (listMoveBy (-1)))
    V.EvKey c [] | c == nextFailureKey -> do
      let ls = Vec.toList $ listElements (s ^. appMainList)
      let listToSearch = case listSelectedElement (s ^. appMainList) of
            Just (i, MainListElem {}) -> let (front, back) = L.splitAt (i + 1) (zip [0..] ls) in back <> front
            Nothing -> zip [0..] ls
      case L.find (isFailureStatus . status . snd) listToSearch of
        Nothing -> continue s
        Just (i', _) -> continue (s & appMainList %~ (listMoveTo i'))
    V.EvKey c [] | c == previousFailureKey -> do
      let ls = Vec.toList $ listElements (s ^. appMainList)
      let listToSearch = case listSelectedElement (s ^. appMainList) of
            Just (i, MainListElem {}) -> let (front, back) = L.splitAt i (zip [0..] ls) in (L.reverse front) <> (L.reverse back)
            Nothing -> L.reverse (zip [0..] ls)
      case L.find (isFailureStatus . status . snd) listToSearch of
        Nothing -> continue s
        Just (i', _) -> continue (s & appMainList %~ (listMoveTo i'))
    V.EvKey c [] | c == closeNodeKey -> modifyOpen s (const False)
    V.EvKey c [] | c == openNodeKey -> modifyOpen s (const True)
    V.EvKey c [] | c `elem` toggleKeys -> modifyToggled s not

    -- Column 2
    V.EvKey c [] | c == cancelAllKey -> do
      liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
      continue s
    V.EvKey c [] | c == cancelSelectedKey -> withContinueS $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_, MainListElem {..}) -> liftIO $
        (readTVarIO $ runTreeStatus node) >>= \case
          Running {..} -> cancel statusAsync
          _ -> return ()
    V.EvKey c [] | c == runAllKey -> withContinueS $ do
      when (all (not . isRunning . runTreeStatus . runNodeCommon) (s ^. appRunTree)) $ liftIO $ do
        mapM_ clearRecursively (s ^. appRunTreeBase)
        void $ async $ void $ runNodesSequentially (s ^. appRunTreeBase) (s ^. appBaseContext)
    V.EvKey c [] | c == runSelectedKey -> withContinueS $
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_, MainListElem {..}) -> case status of
        Running {} -> return ()
        _ -> do
          -- Get the set of IDs for only this node's ancestors and children
          let ancestorIds = S.fromList $ toList $ runTreeAncestors node
          let childIds = S.fromList $ extractValues' (runTreeId . runNodeCommon) runNode
          let allIds = ancestorIds <> childIds
          -- Clear the status of all affected nodes
          liftIO $ mapM_ (clearRecursivelyWhere (\x -> runTreeId x `S.member` allIds)) (s ^. appRunTreeBase)
          -- Start a run for all affected nodes
          let bc = (s ^. appBaseContext) { baseContextOnlyRunIds = Just allIds }
          void $ liftIO $ async $ void $ runNodesSequentially (s ^. appRunTreeBase) bc
    V.EvKey c [] | c == clearResultsKey -> withContinueS $ do
      liftIO $ mapM_ clearRecursively (s ^. appRunTreeBase)
    V.EvKey c [] | c == openSelectedFolderInFileExplorer -> withContinueS $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {folderPath}) ->
        whenJust folderPath $ liftIO . openFileExplorerFolderPortable
    V.EvKey c [] | c == openTestRootKey -> withContinueS $
      whenJust (baseContextRunRoot (s ^. appBaseContext)) $ liftIO . openFileExplorerFolderPortable

    -- Column 3
    V.EvKey c [] | c == cycleVisibilityThresholdKey -> do
      let currentIndex = L.elemIndex (s ^. appVisibilityThreshold) (s ^. appVisibilityThresholdSteps)
      case currentIndex of
        Nothing -> continue s -- Should never happen
        Just index -> do
          let newIndex = (index + 1) `mod` (length (s ^. appVisibilityThresholdSteps))
          let newVisibilityThreshold = (s ^. appVisibilityThresholdSteps) !! newIndex
          continue $ s
            & appVisibilityThreshold .~ newVisibilityThreshold
            & updateFilteredTree
    V.EvKey c [] | c == toggleShowRunTimesKey -> continue $ s
      & appShowRunTimes %~ not
    V.EvKey c [] | c `elem` [V.KEsc, exitKey]-> do
      -- Cancel everything and wait for cleanups
      liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
      _ <- forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
      halt s
    V.EvKey c [] | c == debugKey -> continue (s & appLogLevel .~ Just LevelDebug)
    V.EvKey c [] | c == infoKey -> continue (s & appLogLevel .~ Just LevelInfo)
    V.EvKey c [] | c == warnKey -> continue (s & appLogLevel .~ Just LevelWarn)
    V.EvKey c [] | c == errorKey -> continue (s & appLogLevel .~ Just LevelError)
    V.EvKey c@(V.KChar ch) [V.MMeta] | c `elem` (fmap V.KChar ['0'..'9']) -> do
      let num :: Int = read [ch]
      liftIO $ openToDepth (s ^. (appMainList . listElementsL)) num
      continue s

    ev -> handleEventLensed s appMainList handleListEvent ev >>= continue

  where withContinueS action = action >> continue s
appEvent s _ = continue s

modifyToggled s f = case listSelectedElement (s ^. appMainList) of
  Nothing -> continue s
  Just (i, MainListElem {..}) -> do
    liftIO $ atomically $ modifyTVar (runTreeToggled node) f
    continue s

modifyOpen s f = case listSelectedElement (s ^. appMainList) of
  Nothing -> continue s
  Just (i, MainListElem {..}) -> do
    liftIO $ atomically $ modifyTVar (runTreeOpen node) f
    continue s

openIndices :: [RunNode context] -> Seq.Seq Int -> IO ()
openIndices nodes openSet =
  atomically $ forM_ (concatMap getCommons nodes) $ \node ->
    when ((runTreeId node) `elem` (toList openSet)) $
      modifyTVar (runTreeOpen node) (const True)

openToDepth :: (Foldable t) => t MainListElem -> Int -> IO ()
openToDepth elems thresh =
  atomically $ forM_ elems $ \(MainListElem {..}) ->
    if | (depth < thresh) -> modifyTVar (runTreeOpen node) (const True)
       | otherwise -> modifyTVar (runTreeOpen node) (const False)

setInitialFolding :: InitialFolding -> [RunNode BaseContext] -> IO ()
setInitialFolding InitialFoldingAllOpen rts = return ()
setInitialFolding InitialFoldingAllClosed rts =
  atomically $ forM_ (concatMap getCommons rts) $ \(RunNodeCommonWithStatus {..}) ->
    modifyTVar runTreeOpen (const False)
setInitialFolding (InitialFoldingTopNOpen n) rts =
  atomically $ forM_ (concatMap getCommons rts) $ \(RunNodeCommonWithStatus {..}) ->
    when (Seq.length runTreeAncestors > n) $
      modifyTVar runTreeOpen (const False)

updateFilteredTree :: AppState -> AppState
updateFilteredTree s = s
  & appRunTreeFiltered .~ fmap snd pairs
  & appMainList %~ listReplace (treeToVector pairs) (listSelected $ s ^. appMainList)
  where pairs = zip (filterRunTree (s ^. appVisibilityThreshold) (s ^. appRunTreeBase))
                    (filterRunTree (s ^. appVisibilityThreshold) (s ^. appRunTree))

-- * Clearing

clearRecursively :: RunNode context -> IO ()
clearRecursively = mapM_ clearCommon . getCommons

clearRecursivelyWhere :: (RunNodeCommon -> Bool) -> RunNode context -> IO ()
clearRecursivelyWhere f = mapM_ clearCommon . filter f . getCommons

clearCommon :: RunNodeCommon -> IO ()
clearCommon (RunNodeCommonWithStatus {..}) = atomically $ do
  writeTVar runTreeStatus NotStarted
  writeTVar runTreeLogs mempty
