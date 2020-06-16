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
import Data.Text
import Data.Time.Clock
import Test.Sandwich.Types.Example

data Status l = NotStarted
              | Running { statusStartTime :: UTCTime
                        , statusLogs :: l }
              | Done { statusStartTime :: UTCTime
                     , statusEndTime :: UTCTime
                     , statusLogs :: l
                     , statusResult :: Result }
  deriving Show

data RunTreeWithStatus a =
  RunTreeGroup { runTreeLabel :: String
               , runTreeStatus :: a
               , runTreeIsContextManager :: Bool
               , runTreeChildren :: [RunTreeWithStatus a]
               , runTreeAsync :: Async Result
               }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeStatus :: a
                  , runTreeAsync :: Async Result
                  }
  deriving (Functor)

type RunTree = RunTreeWithStatus (IORef (Status (IORef [Text])))
type RunTreeFixed = RunTreeWithStatus (Status [Text])

fixRunTree :: RunTree -> IO RunTreeFixed
fixRunTree (RunTreeSingle {..}) = do
  status <- readIORef runTreeStatus

  status' <- case status of
    Running {statusLogs} -> do
      logs <- readIORef statusLogs
      return $ status { statusLogs = logs }
    Done {statusLogs} -> do
      logs <- readIORef statusLogs
      return $ status { statusLogs = logs }
    NotStarted -> return NotStarted

  return $ RunTreeSingle {runTreeStatus=status', ..}
fixRunTree (RunTreeGroup {..}) = do
  status <- readIORef runTreeStatus

  status' <- case status of
    Running {statusLogs} -> do
      logs <- readIORef statusLogs
      return $ status { statusLogs = logs }
    Done {statusLogs} -> do
      logs <- readIORef statusLogs
      return $ status { statusLogs = logs }
    NotStarted -> return NotStarted

  children <- forM runTreeChildren fixRunTree

  return $ RunTreeGroup {
    runTreeStatus = status'
    , runTreeChildren = children
    , ..
    }
