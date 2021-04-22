{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sandwich.Types.RunTree where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS8
import Data.Sequence hiding ((:>))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import GHC.Stack
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer


data Status = NotStarted
            | Running { statusStartTime :: UTCTime
                      , statusAsync :: Async Result }
            | Done { statusStartTime :: UTCTime
                   , statusEndTime :: UTCTime
                   , statusResult :: Result }
            deriving (Show, Eq)

instance Show (Async Result) where
  show _ = "AsyncResult"


data RunNodeWithStatus context s l t where
  RunNodeBefore :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildren :: [RunNodeWithStatus context s l t]
    , runNodeBefore :: ExampleT context IO ()
    } -> RunNodeWithStatus context s l t
  RunNodeAfter :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildren :: [RunNodeWithStatus context s l t]
    , runNodeAfter :: ExampleT context IO ()
    } -> RunNodeWithStatus context s l t
  RunNodeIntroduce :: (Typeable intro) => {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildrenAugmented :: [RunNodeWithStatus (LabelValue lab intro :> context) s l t]
    , runNodeAlloc :: ExampleT context IO intro
    , runNodeCleanup :: intro -> ExampleT context IO ()
    } -> RunNodeWithStatus context s l t
  RunNodeIntroduceWith :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildrenAugmented :: [RunNodeWithStatus (LabelValue lab intro :> context) s l t]
    , runNodeIntroduceAction :: (intro -> ExampleT context IO [Result]) -> ExampleT context IO ()
    } -> RunNodeWithStatus context s l t
  RunNodeAround :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildren :: [RunNodeWithStatus context s l t]
    , runNodeActionWith :: ExampleT context IO [Result] -> ExampleT context IO ()
    } -> RunNodeWithStatus context s l t
  RunNodeDescribe :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildren :: [RunNodeWithStatus context s l t]
    } -> RunNodeWithStatus context s l t
  RunNodeParallel :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeChildren :: [RunNodeWithStatus context s l t]
    } -> RunNodeWithStatus context s l t
  RunNodeIt :: {
    runNodeCommon :: RunNodeCommonWithStatus s l t
    , runNodeExample :: ExampleT context IO ()
    } -> RunNodeWithStatus context s l t

type RunNodeFixed context = RunNodeWithStatus context Status (Seq LogEntry) Bool
type RunNode context = RunNodeWithStatus context (Var Status) (Var (Seq LogEntry)) (Var Bool)

-- * RunNodeCommon

data RunNodeCommonWithStatus s l t = RunNodeCommonWithStatus {
  runTreeLabel :: String
  , runTreeId :: Int
  , runTreeAncestors :: Seq Int
  , runTreeToggled :: t
  , runTreeOpen :: t
  , runTreeStatus :: s
  , runTreeVisible :: Bool
  , runTreeFolder :: Maybe FilePath
  , runTreeVisibilityLevel :: Int
  , runTreeRecordTime :: Bool
  , runTreeLogs :: l
  , runTreeLoc :: Maybe SrcLoc
  } deriving (Show, Eq)

type RunNodeCommonFixed = RunNodeCommonWithStatus Status (Seq LogEntry) Bool
type RunNodeCommon = RunNodeCommonWithStatus (Var Status) (Var (Seq LogEntry)) (Var Bool)

-- * Other

type Var = TVar
data LogEntry = LogEntry { logEntryTime :: UTCTime
                         , logEntryLoc :: Loc
                         , logEntrySource :: LogSource
                         , logEntryLevel :: LogLevel
                         , logEntryStr :: LogStr
                         } deriving (Show, Eq)

-- | Context passed around through the evaluation of a RunTree
data RunTreeContext = RunTreeContext {
  runTreeCurrentFolder :: Maybe FilePath
  , runTreeCurrentAncestors :: Seq Int
  , runTreeIndexInParent :: Int
  , runTreeNumSiblings :: Int
  }

-- * Base context

-- | The base context available to every test node.
-- Contains various paths and timing information.
data BaseContext = BaseContext {
  baseContextPath :: Maybe FilePath
  , baseContextRunRoot :: Maybe FilePath
  , baseContextErrorSymlinksDir :: Maybe FilePath
  , baseContextOptions :: Options
  , baseContextOnlyRunIds :: Maybe (S.Set Int)
  , baseContextTestTimerProfile :: T.Text
  , baseContextTestTimer :: TestTimer
  }

-- | Has-* class for asserting a 'BaseContext' is available.
class HasBaseContext a where
  getBaseContext :: a -> BaseContext
  modifyBaseContext :: a -> (BaseContext -> BaseContext) -> a

instance HasBaseContext BaseContext where
  getBaseContext = id
  modifyBaseContext x f = f x

instance HasBaseContext context => HasBaseContext (intro :> context) where
  getBaseContext (_ :> ctx) = getBaseContext ctx
  modifyBaseContext (intro :> ctx) f = intro :> modifyBaseContext ctx f

-- Timing related
instance HasBaseContext context => HasTestTimer context where
  getTestTimer = baseContextTestTimer <$> getBaseContext

type CoreSpec = Spec BaseContext IO

type TopSpec = forall context. HasBaseContext context => SpecFree context IO ()

-- * Specs with command line options provided

commandLineOptions = Label :: Label "commandLineOptions" (CommandLineOptions a)

-- | Has-* class for asserting a 'CommandLineOptions a' is available.
type HasCommandLineOptions context a = HasLabel context "commandLineOptions" (CommandLineOptions a)

type TopSpecWithOptions = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context ()
  ) => SpecFree context IO ()

type TopSpecWithOptions' a = forall context. (
  HasBaseContext context
  , HasCommandLineOptions context a
  ) => SpecFree context IO ()

-- * Formatter

class Formatter f where
  formatterName :: f -> String
  -- ^ Name of the formatter
  runFormatter :: (MonadIO m, MonadLogger m, MonadUnliftIO m, MonadCatch m) => f -> [RunNode BaseContext] -> Maybe (CommandLineOptions ()) -> BaseContext -> m ()
  -- ^ The main function, executed while the test tree is running
  finalizeFormatter :: (MonadIO m, MonadLogger m, MonadCatch m) => f -> [RunNode BaseContext] -> BaseContext -> m ()
  -- ^ Called after the test tree is completed, can be used to print final results

-- | An existential wrapper around 'Formatter's
data SomeFormatter = forall f. (Formatter f, Typeable f) => SomeFormatter f

-- * Options

-- | Control whether test artifacts are stored to a directory.
data TestArtifactsDirectory =
  TestArtifactsNone
  -- ^ Do not create a test artifacts directory.
  | TestArtifactsFixedDirectory {
      testRootFixed :: FilePath
      }
  -- ^ Use the test artifacts directory at the given path, creating it if necessary.
  | TestArtifactsGeneratedDirectory {
      runsRoot :: FilePath
      -- ^ The root folder under which test run directories will be created.
      , getTestRunDirectoryName :: IO FilePath
      -- ^ Action to generate the new directory name.
      }
  -- ^ Create a new test artifacts directory under '' test artifacts directory at the given path.

newtype TreeFilter = TreeFilter String

type LogFn = Loc -> LogSource -> LogLevel -> LogStr -> IO ()
type LogEntryFormatter = UTCTime -> Loc -> LogSource -> LogLevel -> LogStr -> BS8.ByteString

-- The defaultLogStr formatter weirdly puts information after the message. Use our own
defaultLogEntryFormatter :: LogEntryFormatter
defaultLogEntryFormatter ts loc src level msg = fromLogStr $
  toLogStr (BS8.pack $ formatTime defaultTimeLocale "%F %X%4Q %Z" ts)
  <> " ["
  <> defaultLogLevelStr level
  <> "] ("
  <> toLogStr src
  <> ") "
  <> (if isDefaultLoc loc then "" else "@(" <> toLogStr (BS8.pack $ fileLocStr loc) <> ") ")
  <> msg
  <> "\n"

  where
    defaultLogLevelStr :: LogLevel -> LogStr
    defaultLogLevelStr level = case level of
      LevelOther t -> toLogStr t
      _ -> toLogStr $ BS8.pack $ Prelude.drop 5 $ show level

    isDefaultLoc :: Loc -> Bool
    isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)) = True
    isDefaultLoc _ = False

    fileLocStr loc = (loc_package loc) ++ ':' : (loc_module loc) ++
      ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start

data TestTimerType =
  NullTestTimerType
  -- ^ Don't run a test timer
  | SpeedScopeTestTimerType { speedScopeTestTimerWriteRawTimings :: Bool
                              -- ^ Whether to write an additional file with line-by-line timing events, which can be useful for debugging timer issues.
                            }
  -- ^ Test timer that outputs its results in <https://www.speedscope.app/ SpeedScope> JSON format. Also outputs a file with raw timing data in a simple event-based format.

-- | All the options controlling a test run.
data Options = Options {
  optionsTestArtifactsDirectory :: TestArtifactsDirectory
  -- ^ Where to save test artifacts (logs, screenshots, failure reports, etc.).
  , optionsSavedLogLevel :: Maybe LogLevel
  -- ^ Minimum test log level to save (has no effect if 'optionsTestArtifactsDirectory' is 'TestArtifactsNone').
  , optionsMemoryLogLevel :: Maybe LogLevel
  -- ^ Test log level to store in memory while tests are running. (These logs are presented in formatters, etc.).
  , optionsLogFormatter :: LogEntryFormatter
  -- ^ Formatter function for log entries.
  , optionsFilterTree :: Maybe TreeFilter
  -- ^ Filter to apply to the text tree before running.
  , optionsDryRun :: Bool
  -- ^ Whether to skip actually launching the tests. This is useful if you want to see the set of the tests that would be run, or start them manually in the terminal UI.
  , optionsFormatters :: [SomeFormatter]
  -- ^ Which formatters to use to output the results of the tests.
  , optionsProjectRoot :: Maybe FilePath
  -- ^ An optional absolute path to the root of the project being tested (i.e. the folder where the cabal file is found).
  -- This is useful to provide when the current working directory does not match the project root, for example in multi-project Stack setups.
  -- We use this hint to connect 'CallStack' paths (which are relative to the project root) to their actual path on disk.
  , optionsTestTimerType :: TestTimerType
  -- ^ Whether to enable the test timer. When the test timer is present, timing information will be emitted to the project root (if present).
  }


-- | A wrapper type for exceptions with attached callstacks. Haskell doesn't currently offer a way
-- to reliably get a callstack from an exception, but if you can throw (or catch+rethrow) this type
-- then we'll unwrap it and present the callstack nicely.
data SomeExceptionWithCallStack = forall e. Exception e => SomeExceptionWithCallStack e CallStack
instance Show SomeExceptionWithCallStack where
  showsPrec p (SomeExceptionWithCallStack e _) = showsPrec p e
instance Exception SomeExceptionWithCallStack
