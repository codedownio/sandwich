{-# LANGUAGE TemplateHaskell #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick.Widgets.List as L
import Data.Text
import Lens.Micro.TH
import Test.Sandwich.Types.RunTree


data AppEvent = RunTreeUpdated [RunTreeFixed]

data MainListElem = MainListElem {
  label :: String
  , folded :: Bool
  , status :: Status [Text]
  , isContextManager :: Bool
  } deriving Show

data AppState = AppState {
  _appShowContextManagers :: Bool
  , _appShowRunTimes :: Bool
  , _appRunTree :: [RunTreeFixed]
  , _appRunTreeFiltered :: [RunTreeFixed]
  , _appMainList :: L.List () MainListElem
  }

makeLenses ''AppState
