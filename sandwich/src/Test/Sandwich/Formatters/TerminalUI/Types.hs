{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick.Widgets.List as L
import Control.Monad.Logger
import Data.Sequence
import Lens.Micro.TH
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


data TerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold :: Int
  , terminalUIShowRunTimes :: Bool
  , terminalUILogLevel :: Maybe LogLevel
  }

defaultTerminalUIFormatter :: TerminalUIFormatter
defaultTerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold = 0
  , terminalUIShowRunTimes = True
  , terminalUILogLevel = Just LevelWarn
  }


data AppEvent = RunTreeUpdated [RunNodeFixed BaseContext]

data MainListElem = MainListElem {
  label :: String
  , depth :: Int
  , toggled :: Bool
  , open :: Bool
  , status :: Status
  , logs :: Seq LogEntry
  , isContextManager :: Bool
  , visibilityLevel :: Int
  , folderPath :: Maybe FilePath
  , node :: RunNodeCommon
  , runNode :: SomeRunNode
  }

data SomeRunNode = forall context s l t. SomeRunNode { unSomeRunNode :: RunNodeWithStatus context s l t }

data ClickableName = ColorBar | ListRow Int | MainList
  deriving (Show, Ord, Eq)

data AppState = AppState {
  _appRunTreeBase :: [RunNode BaseContext]
  , _appRunTree :: [RunNodeFixed BaseContext]
  , _appRunTreeFiltered :: [RunNodeFixed BaseContext]
  , _appMainList :: L.List ClickableName MainListElem
  , _appBaseContext :: BaseContext

  , _appVisibilityThreshold :: Int
  , _appLogLevel :: Maybe LogLevel
  , _appShowRunTimes :: Bool
  }

makeLenses ''AppState


extractValues' :: (forall context s l t. RunNodeWithStatus context s l t -> a) -> SomeRunNode -> [a]
extractValues' f (SomeRunNode n@(RunNodeIt {})) = [f n]
extractValues' f (SomeRunNode n@(RunNodeIntroduce {runNodeChildrenAugmented})) = (f n) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues' f (SomeRunNode n@(RunNodeIntroduceWith {runNodeChildrenAugmented})) = (f n) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues' f (SomeRunNode n) = (f n) : (concatMap (extractValues f) (runNodeChildren n))
