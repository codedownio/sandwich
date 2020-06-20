{-# LANGUAGE TemplateHaskell #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick.Widgets.List as L
import Data.Sequence
import Lens.Micro.TH
import Test.Sandwich.Types.RunTree


data AppEvent = RunTreeUpdated [RunTreeFixed]

data MainListElem = MainListElem {
  label :: String
  , depth :: Int
  , toggled :: Bool
  , status :: Status
  , logs :: Seq LogEntry
  , isContextManager :: Bool
  , node :: RunTree
  }

data AppState = AppState {
  _appRunTreeBase :: [RunTree]
  , _appRunTree :: [RunTreeFixed]
  , _appRunTreeFiltered :: [RunTreeFixed]
  , _appMainList :: L.List () MainListElem

  , _appShowContextManagers :: Bool
  , _appShowRunTimes :: Bool
  }

makeLenses ''AppState
