{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Types.RunTree where

import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Sequence
import Data.Text
import Data.Time.Clock
import Test.Sandwich.Types.Example

data Status = NotStarted
            | Running { statusStartTime :: UTCTime }
            | Done { statusStartTime :: UTCTime
                   , statusEndTime :: UTCTime
                   , statusResult :: Result }
  deriving Show

data RunTreeWithStatus a l =
  RunTreeGroup { runTreeLabel :: String
               , runTreeStatus :: a
               , runTreeIsContextManager :: Bool
               , runTreeChildren :: [RunTreeWithStatus a l]
               , runTreeLogs :: l
               , runTreeAsync :: Async Result
               }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeStatus :: a
                  , runTreeLogs :: l
                  , runTreeAsync :: Async Result
                  }
  deriving (Functor)

type Var = IORef
type RunTree = RunTreeWithStatus (Var Status) (Var (Seq Text))
type RunTreeFixed = RunTreeWithStatus Status (Seq Text)

fixRunTree :: RunTree -> IO RunTreeFixed
fixRunTree (RunTreeSingle {..}) = do
  status <- readIORef runTreeStatus
  logs <- readIORef runTreeLogs

  return $ RunTreeSingle {runTreeStatus=status, runTreeLogs=logs, ..}
fixRunTree (RunTreeGroup {..}) = do
  status <- readIORef runTreeStatus
  logs <- readIORef runTreeLogs

  children <- forM runTreeChildren fixRunTree

  return $ RunTreeGroup {
    runTreeStatus = status
    , runTreeLogs = logs
    , runTreeChildren = children
    , ..
    }
