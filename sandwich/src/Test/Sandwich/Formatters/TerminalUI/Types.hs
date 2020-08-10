{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick.Widgets.List as L
import Control.Monad.Logger
import Data.Sequence
import qualified Data.Text as T
import Data.Time
import Lens.Micro.TH
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree


-- | Initial settings for the TUI interface. All of these settings can be changed interactively.
data TerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold :: Int
  -- ^ The initial visibility threshold to use when the formatter starts.
  , terminalUIInitialFolding :: InitialFolding
  -- ^ The initial folding settings to use when the formatter starts.
  , terminalUIShowRunTimes :: Bool
  -- ^ Whether to show or hide run times.
  , terminalUIShowVisibilityThresholds :: Bool
  -- ^ Whether to show or hide visibility thresholds next to nodes.
  , terminalUILogLevel :: Maybe LogLevel
  -- ^ Log level for test log displays.
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
  , terminalUIShowVisibilityThresholds = False
  , terminalUILogLevel = Just LevelWarn
  }


data AppEvent = RunTreeUpdated [RunNodeFixed BaseContext]

instance Show AppEvent where
  show (RunTreeUpdated {}) = "<RunTreeUpdated>"

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

data ClickableName = ColorBar | ListRow Int | MainList | InnerViewport T.Text
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
  , _appShowVisibilityThresholds :: Bool
  }

makeLenses ''AppState


extractValues' :: (forall context s l t. RunNodeWithStatus context s l t -> a) -> SomeRunNode -> [a]
extractValues' f (SomeRunNode n@(RunNodeIt {})) = [f n]
extractValues' f (SomeRunNode n@(RunNodeIntroduce {runNodeChildrenAugmented})) = (f n) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues' f (SomeRunNode n@(RunNodeIntroduceWith {runNodeChildrenAugmented})) = (f n) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues' f (SomeRunNode n) = (f n) : (concatMap (extractValues f) (runNodeChildren n))
