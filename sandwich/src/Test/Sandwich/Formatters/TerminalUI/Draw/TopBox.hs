{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Draw.TopBox (
  topBox
  ) where

import Brick
import qualified Brick.Widgets.List as L
import qualified Data.List as L
import Data.Maybe
import Lens.Micro
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Keys
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree


topBox app = hBox [columnPadding settingsColumn
                  , columnPadding actionsColumn
                  , columnPadding otherActionsColumn]
  where
    settingsColumn = keybindingBox [keyIndicator (unKChar nextKey : "/↑") "Next"
                                   , keyIndicator (unKChar previousKey : "/↓") "Previous"
                                   , keyIndicator (unKChar nextFailureKey : "/↑") "Next failure"
                                   , keyIndicator (unKChar previousFailureKey : "/↑") "Previous failure"
                                   , keyIndicator (unKChar closeNodeKey : '/' : [unKChar openNodeKey]) "Open/close nodes"
                                   , keyIndicatorHasSelected app (showKeys toggleKeys) "Toggle selected"]

    actionsColumn = keybindingBox [keyIndicatorSomeTestRunning app (showKey cancelAllKey) "Cancel all"
                                  , keyIndicatorSelectedTestRunning app (showKey cancelSelectedKey) "Cancel selected"
                                  , keyIndicatorNoTestsRunning app (showKey runAllKey) "Run all"
                                  , keyIndicatorSelectedTestDone app (showKey runSelectedKey) "Run selected"
                                  , keyIndicatorAllTestsDone app (showKey clearResultsKey) "Clear results"
                                  , keyIndicatorHasSelectedAndFolder app (showKey openSelectedFolderInFileExplorer) "Open in file explorer"
                                  ]

    otherActionsColumn = keybindingBox [keyIndicator' (showKey cycleVisibilityThresholdKey) (visibilityThresholdWidget app)
                                       , toggleIndicator (app ^. appShowRunTimes) (showKey toggleShowRunTimesKey) "Hide run times" "Show run times"
                                       , keyIndicator "Meta + [0-9]" "Make top # nodes open"
                                       , keyIndicator "q" "Exit"]

visibilityThresholdWidget app = hBox $
  [str "Change visibility threshold ("]
  <> L.intersperse (str ", ") [withAttr (if x == app ^. appVisibilityThreshold then visibilityThresholdSelectedAttr else visibilityThresholdNotSelectedAttr) $ str $ show x | x <- (app ^. appVisibilityThresholdSteps)]
  <> [(str ")")]

columnPadding = padLeft (Pad 1) . padRight (Pad 3) -- . padTop (Pad 1)

keybindingBox = vBox

toggleIndicator True key onMsg _ = keyIndicator key onMsg
toggleIndicator False key _ offMsg = keyIndicator key offMsg

keyIndicator key msg = keyIndicator' key (withAttr hotkeyMessageAttr $ str msg)

keyIndicator' key label = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", label]

keyIndicatorHasSelected app = keyIndicatorContextual app (\s -> isJust $ L.listSelectedElement (s ^. appMainList))

keyIndicatorSelectedTestDone app = keyIndicatorContextual app $ \s -> case L.listSelectedElement (s ^. appMainList) of
  Nothing -> False
  Just (_, MainListElem {..}) -> isDone status
keyIndicatorSelectedTestRunning app = keyIndicatorContextual app $ \s -> case L.listSelectedElement (s ^. appMainList) of
  Nothing -> False
  Just (_, MainListElem {..}) -> isRunning status

keyIndicatorHasSelectedAndFolder app = keyIndicatorContextual app $ \s -> case L.listSelectedElement (s ^. appMainList) of
  Just (_, MainListElem {folderPath=(Just _)}) -> True
  _ -> False

keyIndicatorSomeTestRunning app = keyIndicatorContextual app $ \s -> any (isRunning . runTreeStatus . runNodeCommon) (s ^. appRunTree)
keyIndicatorNoTestsRunning app = keyIndicatorContextual app $ \s -> all (not . isRunning . runTreeStatus . runNodeCommon) (s ^. appRunTree)
keyIndicatorAllTestsDone app = keyIndicatorContextual app $ \s -> all (isDone . runTreeStatus . runNodeCommon) (s ^. appRunTree)
-- keyIndicatorSomeTestsNotDone = keyIndicatorContextual $ \s -> not $ all (isDone . runTreeStatus . runNodeCommon) (s ^. appRunTree)

keyIndicatorContextual app p key msg = case p app of
  True -> hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]
  False -> hBox [str "[", withAttr disabledHotkeyAttr $ str key, str "] ", withAttr disabledHotkeyMessageAttr $ str msg]
