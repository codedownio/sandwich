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
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Data.Sequence
import Data.Time.Clock
import GHC.Stack
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.Spec

data Status = NotStarted
            | Running { statusStartTime :: UTCTime }
            | Done { statusStartTime :: UTCTime
                   , statusEndTime :: UTCTime
                   , statusResult :: Result }
            deriving (Show, Eq)
data RunNodeCommon s l t = RunNodeCommon {
  runTreeLabel :: String
  , runTreeToggled :: t
  , runTreeStatus :: s
  , runTreeFolder :: Maybe FilePath
  , runTreeVisibilityLevel :: Int
  , runTreeLogs :: l
  } deriving Show

data RunNodeWithStatus context s l t where
  RunNodeBefore :: { runNodeCommon :: RunNodeCommon s l t
                   , runNodeChildren :: [RunNodeWithStatus context s l t]
                   , runNodeBefore :: ExampleT context m () } -> RunNodeWithStatus context s l t
  RunNodeAfter :: { runNodeCommon :: RunNodeCommon s l t
                  , runNodeChildren :: [RunNodeWithStatus context s l t]
                  , runNodeAfter :: ExampleT context m () } -> RunNodeWithStatus context s l t
  RunNodeIntroduce :: { runNodeCommon :: RunNodeCommon s l t
                      , runNodeChildrenAugmented :: [RunNodeWithStatus (LabelValue lab intro :> context) s l t]
                      , runNodeAlloc :: ExampleT context m intro
                      , runNodeCleanup :: intro -> ExampleT context m () } -> RunNodeWithStatus context s l t
  RunNodeIntroduceWith :: { runNodeCommon :: RunNodeCommon s l t
                          , runNodeChildrenAugmented :: [RunNodeWithStatus (LabelValue lab intro :> context) s l t]
                          , runNodeIntroduceAction :: ActionWith intro -> ExampleT context m () } -> RunNodeWithStatus context s l t
  RunNodeAround :: { runNodeCommon :: RunNodeCommon s l t
                   , runNodeChildren :: [RunNodeWithStatus context s l t]
                   , runNodeActionWith :: ExampleT context m Result -> ExampleT context m () } -> RunNodeWithStatus context s l t
  RunNodeDescribe :: { runNodeCommon :: RunNodeCommon s l t
                     , runNodeChildren :: [RunNodeWithStatus context s l t] } -> RunNodeWithStatus context s l t
  RunNodeParallel :: { runNodeCommon :: RunNodeCommon s l t
                     , runNodeChildren :: [RunNodeWithStatus context s l t] } -> RunNodeWithStatus context s l t
  RunNodeIt :: { runNodeCommon :: RunNodeCommon s l t
               , runNodeExample :: ExampleT context m () } -> RunNodeWithStatus context s l t

type Var = TVar
data LogEntry = LogEntry { logEntryTime :: UTCTime
                         , logEntryLoc :: Loc
                         , logEntrySource :: LogSource
                         , logEntryLevel :: LogLevel
                         , logEntryStr :: LogStr
                         } deriving (Show, Eq)

type RunNode context = RunNodeWithStatus context Status (Seq LogEntry) Bool
-- type RunNode context = RunNodeWithStatus context (Var Status) (Var (Seq LogEntry)) (Var Bool)
-- type RunTreeFixed = RunTreeWithStatus Status (Seq LogEntry) Bool

-- | Context passed around through the evaluation of a RunTree
data RunTreeContext = RunTreeContext {
  runTreeOptions :: Options
  , runTreeCurrentFolder :: Maybe FilePath
  , runTreeIndexInParent :: Int
  , runTreeNumSiblings :: Int
  }

isFailureStatus :: Status -> Bool
isFailureStatus (Done _ _ stat) = isFailure stat
isFailureStatus _ = False

-- fixRunTree :: RunTree -> STM RunTreeFixed
-- fixRunTree (RunTreeNode {..}) = do
--   status <- readTVar runTreeStatus
--   logs <- readTVar runTreeLogs
--   toggled <- readTVar runTreeToggled

--   children <- forM runTreeChildren fixRunTree

--   return $ RunTreeNode {
--     runTreeStatus = status
--     , runTreeLogs = logs
--     , runTreeToggled = toggled
--     , runTreeChildren = children
--     , ..
--     }

getCallStackFromStatus :: Status -> Maybe CallStack
getCallStackFromStatus NotStarted {} = Nothing
getCallStackFromStatus Running {} = Nothing
getCallStackFromStatus Done {statusResult} = getCallStackFromResult statusResult

getCallStackFromResult :: Result -> Maybe CallStack
getCallStackFromResult (Success {}) = Nothing
getCallStackFromResult (Failure (Reason x _)) = x
getCallStackFromResult (Failure (ExpectedButGot x _ _)) = x
getCallStackFromResult (Failure (DidNotExpectButGot x _)) = x
getCallStackFromResult (Failure (Pending x _)) = x
getCallStackFromResult (Failure (GotException {})) = Nothing
getCallStackFromResult (Failure (GetContextException {})) = Nothing
getCallStackFromResult (Failure (GotAsyncException {})) = Nothing

isDone :: Status -> Bool
isDone (Done {}) = True
isDone _ = False
