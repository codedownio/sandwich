{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Types.RunTree where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Logger
import Data.Sequence hiding ((:>))
import qualified Data.Set as S
import Data.Time.Clock
import Test.Sandwich.Types.Spec

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
  RunNodeBefore :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                   , runNodeChildren :: [RunNodeWithStatus context s l t]
                   , runNodeBefore :: ExampleT context IO () } -> RunNodeWithStatus context s l t
  RunNodeAfter :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                  , runNodeChildren :: [RunNodeWithStatus context s l t]
                  , runNodeAfter :: ExampleT context IO () } -> RunNodeWithStatus context s l t
  RunNodeIntroduce :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                      , runNodeChildrenAugmented :: [RunNodeWithStatus (LabelValue lab intro :> context) s l t]
                      , runNodeAlloc :: ExampleT context IO intro
                      , runNodeCleanup :: intro -> ExampleT context IO () } -> RunNodeWithStatus context s l t
  RunNodeIntroduceWith :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                          , runNodeChildrenAugmented :: [RunNodeWithStatus (LabelValue lab intro :> context) s l t]
                          , runNodeIntroduceAction :: (intro -> ExampleT context IO [Result]) -> ExampleT context IO () } -> RunNodeWithStatus context s l t
  RunNodeAround :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                   , runNodeChildren :: [RunNodeWithStatus context s l t]
                   , runNodeActionWith :: ExampleT context IO [Result] -> ExampleT context IO () } -> RunNodeWithStatus context s l t
  RunNodeDescribe :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                     , runNodeChildren :: [RunNodeWithStatus context s l t] } -> RunNodeWithStatus context s l t
  RunNodeParallel :: { runNodeCommon :: RunNodeCommonWithStatus s l t
                     , runNodeChildren :: [RunNodeWithStatus context s l t] } -> RunNodeWithStatus context s l t
  RunNodeIt :: { runNodeCommon :: RunNodeCommonWithStatus s l t
               , runNodeExample :: ExampleT context IO () } -> RunNodeWithStatus context s l t

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
  , runTreeLogs :: l
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

data BaseContext = BaseContext { baseContextPath :: Maybe FilePath
                               , baseContextRunRoot :: Maybe FilePath
                               , baseContextErrorSymlinksDir :: Maybe FilePath
                               , baseContextErrorCounter :: MVar Int
                               , baseContextOptions :: Options
                               , baseContextOnlyRunIds :: Maybe (S.Set Int) }

class HasBaseContext a where
  getBaseContext :: a -> BaseContext
  modifyBaseContext :: a -> (BaseContext -> BaseContext) -> a

instance HasBaseContext BaseContext where
  getBaseContext = id
  modifyBaseContext x f = f x

instance HasBaseContext context => HasBaseContext (intro :> context) where
  getBaseContext (_ :> ctx) = getBaseContext ctx
  modifyBaseContext (intro :> ctx) f = intro :> modifyBaseContext ctx f

type TopSpec = Spec BaseContext

-- * Formatter

class Formatter f where
  runFormatter :: f -> [RunNode BaseContext] -> BaseContext -> IO ()

data SomeFormatter = forall f. (Formatter f) => SomeFormatter f

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

data Options = Options {
  optionsTestArtifactsDirectory :: TestArtifactsDirectory
  -- ^ Where to save test artifacts (logs, screenshots, failure reports, etc.).
  , optionsSavedLogLevel :: Maybe LogLevel
  -- ^ Minimum test log level to save (has no effect if 'optionsTestArtifactsDirectory' is 'TestArtifactsNone').
  , optionsMemoryLogLevel :: Maybe LogLevel
  -- ^ Test log level to store in memory while tests are running. (These logs are presented in formatters, etc.).
  , optionsFilterTree :: Maybe TreeFilter
  -- ^ Filter to apply to the text tree before running.
  , optionsDryRun :: Bool
  -- ^ Whether to skip actually launching the tests. This is useful if you want to see the set of the tests that would be run, or start them manually in the terminal UI.
  , optionsFormatters :: [SomeFormatter]
  -- ^ Which formatters to use to output the results of the tests.
  }
