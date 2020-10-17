{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
-- |

module Test.Sandwich.Formatters.TerminalUI (
  defaultTerminalUIFormatter
  , terminalUIVisibilityThreshold
  , terminalUIShowRunTimes
  , terminalUIShowVisibilityThresholds
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
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import Data.Time
import qualified Data.Vector as Vec
import GHC.Stack
import qualified Graphics.Vty as V
import Lens.Micro
import Safe
import System.FilePath
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.CrossPlatform
import Test.Sandwich.Formatters.TerminalUI.Draw
import Test.Sandwich.Formatters.TerminalUI.Filter
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Interpreters.StartTree
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util


instance Formatter TerminalUIFormatter where
  formatterName _ = "terminal-ui-formatter"
  runFormatter = runApp
  finalize _ _ _ = return ()

runApp :: (MonadIO m, MonadLogger m) => TerminalUIFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
runApp (TerminalUIFormatter {..}) rts baseContext = liftIO $ do
  startTime <- getCurrentTime

  liftIO $ setInitialFolding terminalUIInitialFolding rts

  rtsFixed <- atomically $ mapM fixRunTree rts

  let initialState = updateFilteredTree $
        AppState {
          _appRunTreeBase = rts
          , _appRunTree = rtsFixed
          , _appMainList = list MainList mempty 1
          , _appBaseContext = baseContext

          , _appStartTime = startTime
          , _appTimeSinceStart = 0

          , _appVisibilityThresholdSteps = L.sort $ L.nub $ terminalUIVisibilityThreshold : (fmap runTreeVisibilityLevel $ concatMap getCommons rts)
          , _appVisibilityThreshold = terminalUIVisibilityThreshold

          , _appLogLevel = terminalUILogLevel
          , _appShowRunTimes = terminalUIShowRunTimes
          , _appShowFileLocations = terminalUIShowFileLocations
          , _appShowVisibilityThresholds = terminalUIShowVisibilityThresholds

          , _appOpenInEditor = terminalUIOpenInEditor
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
      let allCommons = concatMap getCommons $ s ^. appRunTree
      let index = max 0 $ min (length allCommons - 1) $ round $ percent * (fromIntegral $ (length allCommons - 1))
      -- A subsequent RunTreeUpdated will pick up the new open nodes
      liftIO $ openIndices (s ^. appRunTreeBase) (runTreeAncestors $ allCommons !! index)
      continue $ s
        & appMainList %~ (listMoveTo index)
        & updateFilteredTree

appEvent s evt@(MouseDown (ListRow i) V.BScrollUp _ _) = do
  vScrollBy (viewportScroll MainList) (-1)
  continue s
appEvent s evt@(MouseDown (ListRow i) V.BScrollDown _ _) = do
  vScrollBy (viewportScroll MainList) 1
  continue s
appEvent s evt@(MouseDown (ListRow i) V.BLeft _ _) = do
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
    V.EvKey c@(V.KChar ch) [V.MMeta] | c `elem` (fmap V.KChar ['0'..'9']) -> do
      let num :: Int = read [ch]
      liftIO $ openToDepth (s ^. (appMainList . listElementsL)) num
      continue s
    V.EvKey c [] | c `elem` toggleKeys -> modifyToggled s not

    -- Scrolling in toggled items
    -- Wanted to make these uniformly Ctrl+whatever, but Ctrl+PageUp/PageDown was causing it to get KEsc and exit (?)
    V.EvKey V.KUp [V.MCtrl] -> withScroll s $ flip vScrollBy (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> withScroll s $ flip vScrollBy (-1)
    V.EvKey V.KDown [V.MCtrl] -> withScroll s $ flip vScrollBy 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> withScroll s $ flip vScrollBy 1
    V.EvKey (V.KChar 'v') [V.MMeta] -> withScroll s $ flip vScrollPage Up
    V.EvKey (V.KChar 'v') [V.MCtrl] -> withScroll s $ flip vScrollPage Down
    V.EvKey V.KHome [V.MCtrl] -> withScroll s vScrollToBeginning
    V.EvKey V.KEnd [V.MCtrl] -> withScroll s vScrollToEnd

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
          case findRunNodeChildrenById ident (s ^. appRunTree) of
            Nothing -> return ()
            Just childIds -> do
              let allIds = ancestorIds <> childIds
              -- Clear the status of all affected nodes
              liftIO $ mapM_ (clearRecursivelyWhere (\x -> runTreeId x `S.member` allIds)) (s ^. appRunTreeBase)
              -- Start a run for all affected nodes
              let bc = (s ^. appBaseContext) { baseContextOnlyRunIds = Just allIds }
              void $ liftIO $ async $ void $ runNodesSequentially (s ^. appRunTreeBase) bc
    V.EvKey c [] | c == clearSelectedKey -> withContinueS $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_, MainListElem {..}) -> case status of
        Running {} -> return ()
        _ -> case findRunNodeChildrenById ident (s ^. appRunTree) of
          Nothing -> return ()
          Just childIds -> liftIO $ mapM_ (clearRecursivelyWhere (\x -> runTreeId x `S.member` childIds)) (s ^. appRunTreeBase)
    V.EvKey c [] | c == clearAllKey -> withContinueS $ do
      liftIO $ mapM_ clearRecursively (s ^. appRunTreeBase)
    V.EvKey c [] | c == openSelectedFolderInFileExplorer -> withContinueS $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {folderPath}) ->
        whenJust folderPath $ liftIO . openFileExplorerFolderPortable
    V.EvKey c [] | c == openTestRootKey -> withContinueS $
      whenJust (baseContextRunRoot (s ^. appBaseContext)) $ liftIO . openFileExplorerFolderPortable
    V.EvKey c [] | c == openTestInEditorKey -> withContinueS $
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {node}) ->
        whenJust (runTreeLoc node) $ openSrcLoc s
    V.EvKey c [] | c == openLogsInEditorKey -> withContinueS $
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {node}) -> do
        whenJust (runTreeFolder node) $ \dir -> do
          liftIO $ (s ^. appOpenInEditor) $ SrcLoc {
            srcLocPackage = ""
            , srcLocModule = ""
            , srcLocFile = dir </> "test_logs.txt"
            , srcLocStartLine = 0
            , srcLocStartCol = 0
            , srcLocEndLine = 0
            , srcLocEndCol = 0
            }
    V.EvKey c [] | c == openFailureInEditorKey -> withContinueS $
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {node, status}) -> case status of
        Done _ _ _ (Failure (failureCallStack -> Just (getCallStack -> ((_, loc):_)))) -> openSrcLoc s loc
        _ -> return ()

    -- Column 3
    V.EvKey c [] | c == cycleVisibilityThresholdKey -> do
      let newVisibilityThreshold =  case [(i, x) | (i, x) <- zip [0..] (s ^. appVisibilityThresholdSteps), x > s ^. appVisibilityThreshold] of
            [] -> 0
            xs -> minimum $ fmap snd xs
      continue $ s
        & appVisibilityThreshold .~ newVisibilityThreshold
        & updateFilteredTree
    V.EvKey c [] | c == toggleShowRunTimesKey -> continue $ s
      & appShowRunTimes %~ not
    V.EvKey c [] | c == toggleFileLocationsKey -> continue $ s
      & appShowFileLocations %~ not
    V.EvKey c [] | c == toggleVisibilityThresholdsKey -> continue $ s
      & appShowVisibilityThresholds %~ not
    V.EvKey c [] | c `elem` [V.KEsc, exitKey]-> do
      -- Cancel everything and wait for cleanups
      liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
      _ <- forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
      halt s
    V.EvKey c [] | c == debugKey -> continue (s & appLogLevel .~ Just LevelDebug)
    V.EvKey c [] | c == infoKey -> continue (s & appLogLevel .~ Just LevelInfo)
    V.EvKey c [] | c == warnKey -> continue (s & appLogLevel .~ Just LevelWarn)
    V.EvKey c [] | c == errorKey -> continue (s & appLogLevel .~ Just LevelError)

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
  & appMainList %~ listReplace elems (listSelected $ s ^. appMainList)
  where filteredTree = filterRunTree (s ^. appVisibilityThreshold) (s ^. appRunTree)
        elems :: Vec.Vector MainListElem = Vec.fromList $ concatMap treeToList (zip filteredTree (s ^. appRunTreeBase))
-- * Clearing

clearRecursively :: RunNode context -> IO ()
clearRecursively = mapM_ clearCommon . getCommons

clearRecursivelyWhere :: (RunNodeCommon -> Bool) -> RunNode context -> IO ()
clearRecursivelyWhere f = mapM_ clearCommon . filter f . getCommons

clearCommon :: RunNodeCommon -> IO ()
clearCommon (RunNodeCommonWithStatus {..}) = atomically $ do
  writeTVar runTreeStatus NotStarted
  writeTVar runTreeLogs mempty

findRunNodeChildrenById :: Int -> [RunNodeFixed context] -> Maybe (S.Set Int)
findRunNodeChildrenById ident rts = headMay $ mapMaybe (findRunNodeChildrenById' ident) rts

findRunNodeChildrenById' :: Int -> RunNodeFixed context -> Maybe (S.Set Int)
findRunNodeChildrenById' ident node | ident == runTreeId (runNodeCommon node) = Just $ S.fromList $ extractValues (runTreeId . runNodeCommon) node
findRunNodeChildrenById' ident (RunNodeIt {}) = Nothing
findRunNodeChildrenById' ident (RunNodeIntroduce {..}) = findRunNodeChildrenById ident runNodeChildrenAugmented
findRunNodeChildrenById' ident (RunNodeIntroduceWith {..}) = findRunNodeChildrenById ident runNodeChildrenAugmented
findRunNodeChildrenById' ident node = findRunNodeChildrenById ident (runNodeChildren node)

withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (_, MainListElem {..}) -> do
      let scroll = viewportScroll (InnerViewport [i|viewport_#{ident}|])
      action scroll

  continue s

openSrcLoc s loc' = do
  -- Try to make the file path in the SrcLoc absolute
  loc <- case isRelative (srcLocFile loc') of
    False -> return loc'
    True -> do
      case optionsProjectRoot (baseContextOptions (s ^. appBaseContext)) of
        Just d -> return $ loc' { srcLocFile = d </> (srcLocFile loc') }
        Nothing -> return loc'

  -- TODO: check if the path exists and show a warning message if not
  -- Maybe choose the first callstack location we can find?
  liftIO $ (s ^. appOpenInEditor) loc
