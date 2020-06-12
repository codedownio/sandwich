{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Interpreters.RunTreeScheduler (runTree, RunTree (..)) where

import Control.Concurrent.Async
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Scheduler
import Data.IORef
import Data.Time.Clock
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Spec

data Status = NotStarted
            | Running UTCTime
            | Done Result

type RunTreeStatus = IORef Status

instance Show RunTreeStatus where
  show _ = "STATUS"

data RunTree =
  RunTreeGroupWithStatus { runTreeLabel :: String
                         , runTreeStatus :: RunTreeStatus
                         , runTreeChildren :: [RunTree]
                         }
  | RunTreeGroup { runTreeLabel :: String
                 , runTreeChildren :: [RunTree]
                 }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeStatus :: RunTreeStatus
                  }
  deriving Show

runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (Async context, Scheduler IO ()) IO [RunTree]

runTree (Free (Before l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  subtree <- runTree subspec
  let tree = RunTreeGroupWithStatus l status subtree
  rest <- runTree next
  return (tree : rest)
  

runTree (Free (Around l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  let asyncContext = undefined
  (_, sched) <- ask
  subtree <- withReaderT (const (asyncContext, sched)) $ runTree subspec
  let tree = RunTreeGroupWithStatus l status subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (Introduce l alloc cleanup subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  let asyncContext = undefined
  (_, sched) <- ask
  subtree <- withReaderT (const (asyncContext, sched)) $ runTree subspec
  let tree = RunTreeGroupWithStatus l status subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (Describe l subspec next)) = do
  subtree <- runTree subspec
  let tree = RunTreeGroup l subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (DescribeParallel l subspec next)) = do
  subtree <- runTree subspec
  let tree = RunTreeGroup l subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (It l ex next)) = do
  -- (ctxAsync, sched) <- ask
  status <- liftIO $ newIORef NotStarted
  -- liftIO $ async $ do
  --   ctx <- wait ctxAsync
  --   scheduleWork sched $ do
  --     startTime <- liftIO getCurrentTime
  --     liftIO $ atomicWriteIORef status (Run startTime)
  --     ret <- ex ctx
  --     atomicWriteIORef status (Done ret)

  let tree = RunTreeSingle l status
  rest <- runTree next
  return (tree : rest)

runTree (Pure _) = return []
