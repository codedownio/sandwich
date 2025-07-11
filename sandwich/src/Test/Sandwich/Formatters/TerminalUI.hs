{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sandwich.Formatters.TerminalUI (
  -- | The terminal UI formatter produces an interactive UI for running tests and inspecting their results.
  defaultTerminalUIFormatter

  -- * Options
  , terminalUIVisibilityThreshold
  , terminalUIShowRunTimes
  , terminalUIShowVisibilityThresholds
  , terminalUILogLevel
  , terminalUIInitialFolding
  , terminalUIDefaultEditor
  , terminalUIOpenInEditor
  , terminalUICustomExceptionFormatters

  -- * Auxiliary types
  , InitialFolding(..)
  , CustomTUIException(..)

  -- * Util
  , isTuiFormatterSupported
  ) where

import Brick as B
import Brick.BChan
import Brick.Widgets.List
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger hiding (logError)
import Control.Monad.Trans
import Control.Monad.Trans.State hiding (get, put)
import Data.Either
import Data.Foldable
import qualified Data.List as L
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.String.Interpolate
import Data.Time
import qualified Data.Vector as Vec
import GHC.Stack
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as V
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
import Test.Sandwich.Logging
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util
import UnliftIO.Exception


instance Formatter TerminalUIFormatter where
  formatterName _ = "terminal-ui-formatter"
  runFormatter = runApp
  finalizeFormatter _ _ _ = return ()

isTuiFormatterSupported :: IO Bool
isTuiFormatterSupported = isRight <$> tryAny (V.mkVty V.defaultConfig)

runApp :: (MonadLoggerIO m) => TerminalUIFormatter -> [RunNode BaseContext] -> Maybe (CommandLineOptions ()) -> BaseContext -> m ()
runApp (TerminalUIFormatter {..}) rts _maybeCommandLineOptions baseContext = do
  startTime <- liftIO getCurrentTime

  liftIO $ setInitialFolding terminalUIInitialFolding rts

  (rtsFixed, initialSomethingRunning) <- liftIO $ atomically $ runStateT (mapM fixRunTree' rts) False

  let initialState = updateFilteredTree $
        AppState {
          _appRunTreeBase = rts
          , _appRunTree = rtsFixed
          , _appMainList = list MainList mempty 1
          , _appBaseContext = baseContext

          , _appStartTime = startTime
          , _appCurrentTime = startTime
          , _appSomethingRunning = initialSomethingRunning

          , _appVisibilityThresholdSteps = L.sort $ L.nub $ terminalUIVisibilityThreshold : fmap runTreeVisibilityLevel (concatMap getCommons rts)
          , _appVisibilityThreshold = terminalUIVisibilityThreshold

          , _appLogLevel = terminalUILogLevel
          , _appShowRunTimes = terminalUIShowRunTimes
          , _appShowFileLocations = terminalUIShowFileLocations
          , _appShowVisibilityThresholds = terminalUIShowVisibilityThresholds

          , _appOpenInEditor = terminalUIOpenInEditor terminalUIDefaultEditor (const $ return ())
          , _appDebug = (const $ return ())
          , _appCustomExceptionFormatters = terminalUICustomExceptionFormatters
        }

  eventChan <- liftIO $ newBChan 10

  logFn <- askLoggerIO

  currentFixedTree <- liftIO $ newTVarIO rtsFixed
  eventAsync <- liftIO $ async $
    forever $ do
      handleAny (\e -> flip runLoggingT logFn (logError [i|Got exception in event async: #{e}|]) >> threadDelay terminalUIRefreshPeriod) $ do
        (newFixedTree, somethingRunning) <- atomically $ flip runStateT False $ do
          currentFixed <- lift $ readTVar currentFixedTree
          newFixed <- mapM fixRunTree' rts
          when (fmap getCommons newFixed == fmap getCommons currentFixed) (lift retry)
          lift $ writeTVar currentFixedTree newFixed
          return newFixed
        writeBChan eventChan (RunTreeUpdated newFixedTree somethingRunning)
        threadDelay terminalUIRefreshPeriod

  let buildVty = do
        v <- V.mkVty V.defaultConfig
        let output = V.outputIface v
        when (V.supportsMode output V.Mouse) $
          liftIO $ V.setMode output V.Mouse True
        return v
  initialVty <- liftIO buildVty

  let updateCurrentTimeForever period = forever $ do
        now <- getCurrentTime
        writeBChan eventChan (CurrentTimeUpdated now)
        threadDelay period

  liftIO $
    (case terminalUIClockUpdatePeriod of Nothing -> id; Just ts -> \action -> withAsync (updateCurrentTimeForever ts) (\_ -> action)) $
      flip onException (cancel eventAsync) $
        void $ customMain initialVty buildVty (Just eventChan) app initialState

app :: App AppState AppEvent ClickableName
app = App {
  appDraw = drawUI
  , appChooseCursor = showFirstCursor
#if MIN_VERSION_brick(1,0,0)
  , appHandleEvent = \event -> get >>= \s -> appEvent s event
  , appStartEvent = return ()
#else
  , appHandleEvent = appEvent
  , appStartEvent = return
#endif
  , appAttrMap = const mainAttrMap
  }

#if MIN_VERSION_brick(1,0,0)
continue :: AppState -> EventM ClickableName AppState ()
continue = put

continueNoChange :: AppState -> EventM ClickableName AppState ()
continueNoChange _ = return ()

doHalt :: p -> EventM n s ()
doHalt _ = halt
#else
continueNoChange :: AppState -> EventM ClickableName (Next AppState)
continueNoChange = continue

doHalt :: p -> EventM n s
doHalt = halt
#endif

#if MIN_VERSION_brick(1,0,0)
appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName AppState ()
#else
appEvent :: AppState -> BrickEvent ClickableName AppEvent -> EventM ClickableName (Next AppState)
#endif
appEvent s (AppEvent (RunTreeUpdated newTree somethingRunning)) = do
  now <- liftIO getCurrentTime
  continue $ s
    & appRunTree .~ newTree
    & appCurrentTime .~ now
    & appSomethingRunning .~ somethingRunning
    & updateFilteredTree
appEvent s (AppEvent (CurrentTimeUpdated ts)) = do
  continue $ case (s ^. appSomethingRunning) of
    True -> s & appCurrentTime .~ ts
    False -> s

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

appEvent s (MouseDown (ListRow _i) V.BScrollUp _ _) = do
  vScrollBy (viewportScroll MainList) (-1)
  continueNoChange s
appEvent s (MouseDown (ListRow _i) V.BScrollDown _ _) = do
  vScrollBy (viewportScroll MainList) 1
  continueNoChange s
appEvent s (MouseDown (ListRow n) V.BLeft _ _) = do
  continue (s & appMainList %~ (listMoveTo n))
appEvent s (VtyEvent e) =
  case e of
    -- Column 1
    V.EvKey c [] | c == nextKey -> continue (s & appMainList %~ (listMoveBy 1))
    V.EvKey c [] | c == previousKey -> continue (s & appMainList %~ (listMoveBy (-1)))
    V.EvKey c [] | c == nextFailureKey -> do
      let ls = Vec.toList $ listElements (s ^. appMainList)
      let listToSearch = case listSelectedElement (s ^. appMainList) of
            Just (n, MainListElem {}) -> let (front, back) = L.splitAt (n + 1) (zip [0..] ls) in back <> front
            Nothing -> zip [0..] ls
      case L.find (isFailureStatus . status . snd) listToSearch of
        Nothing -> continue s
        Just (i', _) -> continue (s & appMainList %~ (listMoveTo i'))
    V.EvKey c [] | c == previousFailureKey -> do
      let ls = Vec.toList $ listElements (s ^. appMainList)
      let listToSearch = case listSelectedElement (s ^. appMainList) of
            Just (n, MainListElem {}) -> let (front, back) = L.splitAt n (zip [0..] ls) in (L.reverse front) <> (L.reverse back)
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
    V.EvKey V.KUp [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp (-1)
    V.EvKey (V.KChar 'p') [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp (-1)
    V.EvKey V.KDown [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp 1
    V.EvKey (V.KChar 'n') [V.MCtrl] -> withScroll s $ \vp -> vScrollBy vp 1
    V.EvKey (V.KChar 'v') [V.MMeta] -> withScroll s $ \vp -> vScrollPage vp Up
    V.EvKey (V.KChar 'v') [V.MCtrl] -> withScroll s $ \vp -> vScrollPage vp Down

#ifdef darwin_HOST_OS
    -- This seems okay on macOS, and is a good fallback since Meta+v doesn't seem to work
    V.EvKey (V.KPageUp) [V.MCtrl] -> withScroll s $ \vp -> vScrollPage vp Up
    V.EvKey (V.KPageDown) [V.MCtrl] -> withScroll s $ \vp -> vScrollPage vp Down
#endif

    V.EvKey V.KHome [V.MCtrl] -> withScroll s $ \vp -> vScrollToBeginning vp
    V.EvKey V.KEnd [V.MCtrl] -> withScroll s $ \vp -> vScrollToEnd vp

    -- Column 2
    V.EvKey c [] | c == cancelAllKey -> do
      liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
      continue s
    V.EvKey c [] | c == cancelSelectedKey -> withContinueS s $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_, MainListElem {..}) -> liftIO $
        (readTVarIO $ runTreeStatus node) >>= \case
          Running {..} -> cancel statusAsync
          _ -> return ()
    V.EvKey c [] | c == runAllKey -> do
      now <- liftIO getCurrentTime
      when (all (not . isRunning . runTreeStatus . runNodeCommon) (s ^. appRunTree)) $ liftIO $ do
        mapM_ clearRecursively (s ^. appRunTreeBase)
        void $ async $ void $ runNodesSequentially (s ^. appRunTreeBase) (s ^. appBaseContext)
      continue $ s
        & appStartTime .~ now
        & appCurrentTime .~ now
    V.EvKey c [] | c == runSelectedKey ->
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_, MainListElem {..}) -> case status of
        Running {} -> continue s
        _ -> do
          -- Get the set of IDs for only this node's ancestors and children
          let ancestorIds = S.fromList $ toList $ runTreeAncestors node
          case findRunNodeChildrenById ident (s ^. appRunTree) of
            Nothing -> continue s
            Just childIds -> do
              let allIds = ancestorIds <> childIds
              -- Clear the status of all affected nodes
              liftIO $ mapM_ (clearRecursivelyWhere (\x -> runTreeId x `S.member` allIds)) (s ^. appRunTreeBase)
              -- Start a run for all affected nodes
              now <- liftIO getCurrentTime
              let bc = (s ^. appBaseContext) { baseContextOnlyRunIds = Just allIds }
              void $ liftIO $ async $ void $ runNodesSequentially (s ^. appRunTreeBase) bc
              continue $ s
                & appStartTime .~ now
                & appCurrentTime .~ now
    V.EvKey c [] | c == clearSelectedKey -> withContinueS s $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_, MainListElem {..}) -> case status of
        Running {} -> return ()
        _ -> case findRunNodeChildrenById ident (s ^. appRunTree) of
          Nothing -> return ()
          Just childIds -> liftIO $ mapM_ (clearRecursivelyWhere (\x -> runTreeId x `S.member` childIds)) (s ^. appRunTreeBase)
    V.EvKey c [] | c == clearAllKey -> withContinueS s $ do
      liftIO $ mapM_ clearRecursively (s ^. appRunTreeBase)
    V.EvKey c [] | c == openSelectedFolderInFileExplorer -> withContinueS s $ do
      whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, MainListElem {folderPath}) ->
        whenJust folderPath $ liftIO . openFileExplorerFolderPortable
    V.EvKey c [] | c == openTestRootKey -> withContinueS s $
      whenJust (baseContextRunRoot (s ^. appBaseContext)) $ liftIO . openFileExplorerFolderPortable
    V.EvKey c [] | c == openTestInEditorKey -> case listSelectedElement (s ^. appMainList) of
      Just (_i, MainListElem {node=(runTreeLoc -> Just loc)}) -> openSrcLoc s loc
      _ -> continue s
    V.EvKey c [] | c == openLogsInEditorKey -> case listSelectedElement (s ^. appMainList) of
      Just (_i, MainListElem {node=(runTreeFolder -> Just dir)}) -> do
        let srcLoc = SrcLoc {
          srcLocPackage = ""
          , srcLocModule = ""
          , srcLocFile = dir </> "test_logs.txt"
          , srcLocStartLine = 0
          , srcLocStartCol = 0
          , srcLocEndLine = 0
          , srcLocEndCol = 0
          }
        suspendAndResume ((s ^. appOpenInEditor) srcLoc >> return s)
      _ -> continue s
    V.EvKey c [] | c == openFailureInEditorKey -> do
      case (listSelectedElement (s ^. appMainList)) of
        Nothing -> continue s
        Just (_i, MainListElem {status}) -> case status of
          Done _ _ _ _ (Failure (failureCallStack -> Just (getCallStack -> ((_, loc):_)))) -> openSrcLoc s loc
          _ -> continue s

    -- Column 3
    V.EvKey c [] | c == cycleVisibilityThresholdKey -> do
      let newVisibilityThreshold =  case [(n, x) | (n, x) <- zip [(0 :: Integer)..] (s ^. appVisibilityThresholdSteps)
                                                 , x > s ^. appVisibilityThreshold] of
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
    V.EvKey c [] | c `elem` [V.KEsc, exitKey] -> do
      -- Cancel everything and wait for cleanups
      liftIO $ mapM_ cancelNode (s ^. appRunTreeBase)
      forM_ (s ^. appRunTreeBase) (liftIO . waitForTree)
      doHalt s
    V.EvKey c [] | c == debugKey -> continue (s & appLogLevel ?~ LevelDebug)
    V.EvKey c [] | c == infoKey -> continue (s & appLogLevel ?~ LevelInfo)
    V.EvKey c [] | c == warnKey -> continue (s & appLogLevel ?~ LevelWarn)
    V.EvKey c [] | c == errorKey -> continue (s & appLogLevel ?~ LevelError)

#if MIN_VERSION_brick(1,0,0)
    ev -> zoom appMainList $ handleListEvent ev
#else
    ev -> handleEventLensed s appMainList handleListEvent ev >>= continue
#endif

  where withContinueS s' action = action >> continue s'
#if MIN_VERSION_brick(1,0,0)
appEvent _ _ = return ()
#else
appEvent s _ = continue s
#endif

modifyToggled :: AppState -> (Bool -> Bool) -> EventM ClickableName AppState ()
modifyToggled s f = case listSelectedElement (s ^. appMainList) of
  Nothing -> continue s
  Just (_i, MainListElem {..}) -> do
    liftIO $ atomically $ modifyTVar (runTreeToggled node) f
    continue s

modifyOpen :: AppState -> (Bool -> Bool) -> EventM ClickableName AppState ()
modifyOpen s f = case listSelectedElement (s ^. appMainList) of
  Nothing -> continue s
  Just (_i, MainListElem {..}) -> do
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
setInitialFolding InitialFoldingAllOpen _rts = return ()
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
clearCommon (RunNodeCommonWithStatus {..}) = do
  atomically $ do
    writeTVar runTreeStatus NotStarted
    writeTVar runTreeLogs mempty

  -- TODO: clearing the folders might be better for reproducibility, but it might be more surprising than not doing it.
  -- Also, we'd want to be a little judicious about which folders get cleared -- clearing entire "describe" folders would
  -- blow away unrelated test results. So maybe it's better to not clear, and for tests to just do idempotent things in
  -- their folders.
  -- whenJust runTreeFolder $ \folder -> do
  --   doesDirectoryExist folder >>= \case
  --     False -> return ()
  --     True -> clearDirectoryContents folder
  -- where
  --   clearDirectoryContents :: FilePath -> IO ()
  --   clearDirectoryContents path = do
  --     paths <- listDirectory path
  --     forM_ paths removePathForcibly

findRunNodeChildrenById :: Int -> [RunNodeFixed context] -> Maybe (S.Set Int)
findRunNodeChildrenById ident rts = headMay $ mapMaybe (findRunNodeChildrenById' ident) rts

findRunNodeChildrenById' :: Int -> RunNodeFixed context -> Maybe (S.Set Int)
findRunNodeChildrenById' ident node | ident == runTreeId (runNodeCommon node) = Just $ S.fromList $ extractValues (runTreeId . runNodeCommon) node
findRunNodeChildrenById' _ident (RunNodeIt {}) = Nothing
findRunNodeChildrenById' ident (RunNodeIntroduce {..}) = findRunNodeChildrenById ident runNodeChildrenAugmented
findRunNodeChildrenById' ident (RunNodeIntroduceWith {..}) = findRunNodeChildrenById ident runNodeChildrenAugmented
findRunNodeChildrenById' ident node = findRunNodeChildrenById ident (runNodeChildren node)

#if MIN_VERSION_brick(1,0,0)
withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
#else
withScroll :: AppState -> (ViewportScroll ClickableName -> EventM n ()) -> EventM n (Next AppState)
#endif
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (_, MainListElem {..}) -> do
      let scroll = viewportScroll (InnerViewport [i|viewport_#{ident}|])
      action scroll

#if !MIN_VERSION_brick(1,0,0)
  continue s
#endif

openSrcLoc :: Ord n => AppState -> SrcLoc -> EventM n AppState ()
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
  suspendAndResume (((s ^. appOpenInEditor) loc) >> return s)
