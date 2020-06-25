{-# LANGUAGE NamedFieldPuns #-}
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

data RunTreeWithStatus s l t =
  RunTreeGroup { runTreeLabel :: String
               , runTreeToggled :: t
               , runTreeStatus :: s
               , runTreeFolder :: Maybe FilePath
               , runTreeIsContextManager :: Bool
               , runTreeChildren :: [RunTreeWithStatus s l t]
               , runTreeLogs :: l
               , runTreeAsync :: Async ()
               }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeToggled :: t
                  , runTreeStatus :: s
                  , runTreeFolder :: Maybe FilePath
                  , runTreeLogs :: l
                  , runTreeAsync :: Async ()
                  }
  deriving (Functor, Eq)

type Var = TVar
data LogEntry = LogEntry { logEntryTime :: UTCTime
                         , logEntryLoc :: Loc
                         , logEntrySource :: LogSource
                         , logEntryLevel :: LogLevel
                         , logEntryStr :: LogStr
                         } deriving (Show, Eq)
type RunTree = RunTreeWithStatus (Var Status) (Var (Seq LogEntry)) (Var Bool)
type RunTreeFixed = RunTreeWithStatus Status (Seq LogEntry) Bool

-- | Context passed around through the evaluation of a RunTree
data RunTreeContext context = RunTreeContext {
  runTreeContext :: Async context
  , runTreeOptions :: Options
  , runTreeCurrentFolder :: Maybe FilePath
  , runTreeIndexInParent :: Int
  , runTreeNumSiblings :: Int
  }

fixRunTree :: RunTree -> STM RunTreeFixed
fixRunTree (RunTreeSingle {..}) = do
  status <- readTVar runTreeStatus
  logs <- readTVar runTreeLogs
  toggled <- readTVar runTreeToggled

  return $ RunTreeSingle {
    runTreeStatus=status
    , runTreeLogs=logs
    , runTreeToggled=toggled
    , ..
    }
fixRunTree (RunTreeGroup {..}) = do
  status <- readTVar runTreeStatus
  logs <- readTVar runTreeLogs
  toggled <- readTVar runTreeToggled

  children <- forM runTreeChildren fixRunTree

  return $ RunTreeGroup {
    runTreeStatus = status
    , runTreeLogs = logs
    , runTreeToggled = toggled
    , runTreeChildren = children
    , ..
    }

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
