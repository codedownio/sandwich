{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Interpreters.RunTreeScheduler (
  runTree
  , RunTreeWithStatus(..)
  , RunTree
  , RunTreeFixed
  , Status(..)
  , RunTreeStatus
  , fixRunTree
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Scheduler
import Data.IORef
import Data.Time.Clock
import System.Random
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (Async context, Scheduler IO (), Options) IO [RunTree]

runTree (Free (Before l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ runRandomly status
  subtree <- runTree subspec
  let tree = RunTreeGroup l status subtree
  rest <- runTree next
  return (tree : rest)
  

runTree (Free (Around l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ runRandomly status
  let asyncContext = undefined
  (_, sched, opts) <- ask
  subtree <- withReaderT (const (asyncContext, sched, opts)) $ runTree subspec
  let tree = RunTreeGroup l status subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (Introduce l alloc cleanup subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ runRandomly status
  let asyncContext = undefined
  (_, sched, opts) <- ask
  subtree <- withReaderT (const (asyncContext, sched, opts)) $ runTree subspec
  let tree = RunTreeGroup l status subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (Describe l subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ runRandomly status
  subtree <- runTree subspec
  let tree = RunTreeGroup l status subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (DescribeParallel l subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ runRandomly status
  subtree <- runTree subspec
  let tree = RunTreeGroup l status subtree
  rest <- runTree next
  return (tree : rest)

runTree (Free (It l ex next)) = do
  -- (ctxAsync, sched) <- ask
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ runRandomly status
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




runRandomly status = do
  waitRandom
  now <- getCurrentTime
  atomicWriteIORef status (Running now)
  waitRandom

  finalResult <- randomRIO (1 :: Int, 4) >>= \case
    1 -> return $ Pending Nothing Nothing
    2 -> return $ Failure Nothing NoReason
    _ -> return $ Success
  
  atomicWriteIORef status (Done finalResult)

  where
    waitRandom = do
      seconds <- randomRIO (1, 10)
      threadDelay (seconds * 1000000)

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
