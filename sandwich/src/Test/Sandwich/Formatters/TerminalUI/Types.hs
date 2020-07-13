{-# LANGUAGE TemplateHaskell #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick.Widgets.List as L
import Data.Sequence
import Lens.Micro.TH
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


data AppEvent = RunTreeUpdated [RunNodeFixed BaseContext]

data MainListElem = MainListElem {
  label :: String
  , depth :: Int
  , toggled :: Bool
  , status :: Status
  , logs :: Seq LogEntry
  , isContextManager :: Bool
  , visibilityLevel :: Int
  , folderPath :: Maybe FilePath
  , node :: RunNodeCommon
  }

data AppState = AppState {
  _appRunTreeBase :: [RunNode BaseContext]
  , _appRunTree :: [RunNodeFixed BaseContext]
  , _appRunTreeFiltered :: [RunNodeFixed BaseContext]
  , _appMainList :: L.List () MainListElem
  , _appBaseContext :: BaseContext

  , _appShowContextManagers :: Bool
  , _appShowRunTimes :: Bool
  }

makeLenses ''AppState
