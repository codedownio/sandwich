{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Sandwich.Formatters.TerminalUI.Types where

import qualified Brick as B
import qualified Brick.Widgets.List as L
import Control.Exception
import Control.Monad.Logger
import Data.Sequence
import qualified Data.Text as T
import Data.Time
import GHC.Stack
import Lens.Micro.TH
import Test.Sandwich.Formatters.TerminalUI.OpenInEditor
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree


data TerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold :: Int
  -- ^ The initial visibility threshold to use when the formatter starts.
  , terminalUIInitialFolding :: InitialFolding
  -- ^ The initial folding settings to use when the formatter starts.
  , terminalUIShowRunTimes :: Bool
  -- ^ Whether to show or hide run times.
  , terminalUIShowFileLocations :: Bool
  -- ^ Whether to show or hide the files in which tests are defined.
  , terminalUIShowVisibilityThresholds :: Bool
  -- ^ Whether to show or hide visibility thresholds next to nodes.
  , terminalUILogLevel :: Maybe LogLevel
  -- ^ Log level for test log displays.
  , terminalUIRefreshPeriod :: Int
  -- ^ Time in microseconds between test tree renders. Defaults to 100ms. Can be increased if CPU usage of the UI is too high.
  , terminalUIClockUpdatePeriod :: Maybe Int
  -- ^ Time in microseconds between clock ticks. This causes the app's current time to be updated, which powers the
  -- run time displays and the overall app uptime displayed at the top. Defaults to Just 1s. If Nothing, the clock timer is disabled.
  -- Can be increased if CPU usage of the UI is too high.
  , terminalUIDefaultEditor :: Maybe String
  -- ^ Default value to use for the EDITOR environment variable when one is not provided.
  -- If 'Nothing' and EDITOR can't be found, edit commands will do nothing.
  --
  -- Here are some recommended values, depending on your preferred editor:
  --
  -- * Emacs: @export EDITOR="emacsclient --eval '(progn (find-file FILE) (goto-line LINE) (forward-char (- COLUMN 1)) (recenter))'"@
  -- * Terminal Emacs: @export EDITOR="emacsclient -nw --eval '(progn (find-file FILE) (goto-line LINE) (forward-char (- COLUMN 1)) (recenter))'"@
  -- * Vim: @export EDITOR="vim +LINE"@
  , terminalUIOpenInEditor :: Maybe String -> (T.Text -> IO ()) -> SrcLoc -> IO ()
  -- ^ Callback to open a source location in your editor. By default, finds the command in the EDITOR environment variable
  -- and invokes it with the strings LINE, COLUMN, and FILE replaced with the line number, column, and file path.
  -- If FILE is not found in the string, it will be appended to the command after a space.
  -- It's also passed a debug callback that accepts a 'T.Text'; messages logged with this function will go into the formatter logs.
  , terminalUICustomExceptionFormatters :: CustomExceptionFormatters
  -- ^ Custom exception formatters, used to nicely format custom exception types.
  }

instance Show TerminalUIFormatter where
  show (TerminalUIFormatter {}) = "<TerminalUIFormatter>"

data InitialFolding =
  InitialFoldingAllOpen
  | InitialFoldingAllClosed
  | InitialFoldingTopNOpen Int
  deriving (Show, Eq)

-- | Default settings for the terminal UI formatter.
defaultTerminalUIFormatter :: TerminalUIFormatter
defaultTerminalUIFormatter = TerminalUIFormatter {
  terminalUIVisibilityThreshold = 50
  , terminalUIInitialFolding = InitialFoldingAllOpen
  , terminalUIShowRunTimes = True
  , terminalUIShowFileLocations = False
  , terminalUIShowVisibilityThresholds = False
  , terminalUILogLevel = Just LevelWarn
  , terminalUIRefreshPeriod = 100000
  , terminalUIClockUpdatePeriod = Just 1000000
  , terminalUIDefaultEditor = Just "emacsclient +$((LINE+1)):COLUMN --no-wait"
  , terminalUIOpenInEditor = autoOpenInEditor
  , terminalUICustomExceptionFormatters = []
  }

type CustomExceptionFormatters = [SomeException -> Maybe CustomTUIException]

data CustomTUIException = CustomTUIExceptionMessageAndCallStack T.Text (Maybe CallStack)
                        | CustomTUIExceptionBrick (forall n. B.Widget n)

data AppEvent =
  RunTreeUpdated { runTreeUpdateTree :: [RunNodeFixed BaseContext]
                 , runTreeUpdateSomethingRunning :: Bool }
  | CurrentTimeUpdated { currentTimeUpdatedTs :: UTCTime}

instance Show AppEvent where
  show (RunTreeUpdated {}) = "<RunTreeUpdated>"
  show (CurrentTimeUpdated {}) = "<CurrentTimeUpdated>"

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

  -- | Set at formatter initialization and never changed
  , _appStartTime :: UTCTime
  -- | Only incremented when some test is running
  , _appCurrentTime :: UTCTime
  , _appSomethingRunning :: Bool

  , _appVisibilityThresholdSteps :: [Int]
  , _appVisibilityThreshold :: Int

  , _appLogLevel :: Maybe LogLevel
  , _appShowRunTimes :: Bool
  , _appShowFileLocations :: Bool
  , _appShowVisibilityThresholds :: Bool

  , _appOpenInEditor :: SrcLoc -> IO ()
  , _appDebug :: T.Text -> IO ()
  , _appCustomExceptionFormatters :: CustomExceptionFormatters
  }

makeLenses ''AppState


extractValues' :: (forall context s l t. RunNodeWithStatus context s l t -> a) -> SomeRunNode -> [a]
extractValues' f (SomeRunNode n@(RunNodeIt {})) = [f n]
extractValues' f (SomeRunNode n@(RunNodeIntroduce {runNodeChildrenAugmented})) = (f n) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues' f (SomeRunNode n@(RunNodeIntroduceWith {runNodeChildrenAugmented})) = (f n) : (concatMap (extractValues f) runNodeChildrenAugmented)
extractValues' f (SomeRunNode n) = (f n) : (concatMap (extractValues f) (runNodeChildren n))
