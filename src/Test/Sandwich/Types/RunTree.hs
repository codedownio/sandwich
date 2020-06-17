{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Types.RunTree where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Sequence
import Data.Text
import Data.Time.Clock
import Test.Sandwich.Types.Spec

data Status = NotStarted
            | Running { statusStartTime :: UTCTime }
            | Done { statusStartTime :: UTCTime
                   , statusEndTime :: UTCTime
                   , statusResult :: Result }
  deriving (Show, Eq)

data RunTreeWithStatus a l t =
  RunTreeGroup { runTreeLabel :: String
               , runTreeToggled :: t
               , runTreeStatus :: a
               , runTreeIsContextManager :: Bool
               , runTreeChildren :: [RunTreeWithStatus a l t]
               , runTreeLogs :: l
               , runTreeAsync :: Async Result
               }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeToggled :: t
                  , runTreeStatus :: a
                  , runTreeLogs :: l
                  , runTreeAsync :: Async Result
                  }
  deriving (Functor, Eq)

type Var = TVar
type LogEntry = Text
type RunTree = RunTreeWithStatus (Var Status) (Var (Seq LogEntry)) (Var Bool)
type RunTreeFixed = RunTreeWithStatus Status (Seq LogEntry) Bool

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
