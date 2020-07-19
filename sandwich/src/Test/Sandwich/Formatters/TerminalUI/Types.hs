{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick.Widgets.List as L
import Control.Monad.Logger
import Data.Sequence
import Data.Time
import Lens.Micro.TH
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree


data TerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold :: Int
  -- * The initial visibility threshold to use when the formatter starts. Can be changed interactively.
  , terminalUIInitialFolding :: InitialFolding
  -- * The initial folding settings to use when the formatter starts. Can be changed interactively.
  , terminalUIShowRunTimes :: Bool
  -- * Whether to show or hide run times. Can be changed interactively.
  , terminalUILogLevel :: Maybe LogLevel
  -- * Log level for test log displays. Can be changed interactively.
  }

data InitialFolding =
  InitialFoldingAllOpen
  | InitialFoldingAllClosed
  | InitialFoldingTopNOpen Int
  deriving (Show, Eq)

defaultTerminalUIFormatter :: TerminalUIFormatter
defaultTerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold = 50
  , terminalUIInitialFolding = InitialFoldingAllOpen
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
  , visibilityLevel :: Int
  , folderPath :: Maybe FilePath
  , node :: RunNodeCommon
  , ident :: Int
  }

data SomeRunNode = forall context s l t. SomeRunNode { unSomeRunNode :: RunNodeWithStatus context s l t }

data ClickableName = ColorBar | ListRow Int | MainList
  deriving (Show, Ord, Eq)

data AppState = AppState {
  _appRunTreeBase :: [RunNode BaseContext]
  , _appRunTree :: [RunNodeFixed BaseContext]
  , _appMainList :: L.List ClickableName MainListElem
  , _appBaseContext :: BaseContext

  , _appStartTime :: UTCTime
  , _appTimeSinceStart :: NominalDiffTime

  , _appVisibilityThresholdSteps :: [Int]
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
