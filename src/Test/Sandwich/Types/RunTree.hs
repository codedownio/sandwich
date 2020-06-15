{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Types.RunTree where

import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Test.Sandwich.Types.Example

data Status = NotStarted
            | Running UTCTime
            | Done Result
  deriving Show

type RunTreeStatus = IORef Status

instance Show RunTreeStatus where
  show _ = "STATUS"

data RunTreeWithStatus a =
  RunTreeGroup { runTreeLabel :: String
               , runTreeStatus :: a
               , runTreeIsContextManager :: Bool
               , runTreeChildren :: [RunTreeWithStatus a]
               , runTreeAsync :: Async ()
               }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeStatus :: a
                  , runTreeAsync :: Async ()
                  }
  deriving (Functor)

type RunTree = RunTreeWithStatus (RunTreeStatus)
type RunTreeFixed = RunTreeWithStatus (Status)

fixRunTree :: RunTree -> IO RunTreeFixed
fixRunTree (RunTreeSingle {..}) = do
  status <- readIORef runTreeStatus
  return $ RunTreeSingle {runTreeStatus=status, ..}
fixRunTree (RunTreeGroup {..}) = do
  status <- readIORef runTreeStatus
  children <- forM runTreeChildren fixRunTree
  return $ RunTreeGroup {
    runTreeStatus = status
    , runTreeChildren = children
    , ..
    }
