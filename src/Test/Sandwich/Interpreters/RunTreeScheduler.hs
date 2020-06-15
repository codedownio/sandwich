{-# LANGUAGE StandaloneDeriving #-}
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
  , RunTreeContext(..)
  , fixRunTree
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Scheduler
import Data.IORef
import Data.Time.Clock
import System.Random
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

data RunTreeContext context = RunTreeContext {
  runTreeContext :: Async context
  , runTreeOptions :: Options
  }

type ScheduleMonad context = ReaderT (RunTreeContext context) IO

runTree :: (Show r, Show context) => Free (SpecCommand context) r -> Scheduler IO a -> (ScheduleMonad context) [RunTree]

runTree (Free (Before l f subspec next)) sched = do
  status <- liftIO $ newIORef NotStarted

  groupAsync <- liftIO $ async $ runRandomly status
  -- scheduleWork sched $ do
    -- (ctx, opts) <- ask

  subtree <- runTree subspec sched
  let tree = RunTreeGroup l status subtree groupAsync
  rest <- runTree next sched
  return (tree : rest)
  
runTree (Free (After l f subspec next)) sched = do
  status <- liftIO $ newIORef NotStarted
  groupAsync <- liftIO $ async $ runRandomly status
  subtree <- runTree subspec sched
  let tree = RunTreeGroup l status subtree groupAsync
  rest <- runTree next sched
  return (tree : rest)

runTree (Free (Around l f subspec next)) sched = do
  status <- liftIO $ newIORef NotStarted
  groupAsync <- liftIO $ async $ runRandomly status
  let asyncContext = undefined
  rtc@RunTreeContext {..} <- ask
  subtree <- withReaderT (const rtc) $ runTree subspec sched
  let tree = RunTreeGroup l status subtree groupAsync
  rest <- runTree next sched
  return (tree : rest)

runTree (Free (Introduce l alloc cleanup subspec next)) sched = do
  status <- liftIO $ newIORef NotStarted
  groupAsync <- liftIO $ async $ runRandomly status
  let asyncContext = undefined
  rtc@RunTreeContext {..} <- ask
  subtree <- withReaderT (const (RunTreeContext {})) $ runTree subspec sched
  let tree = RunTreeGroup l status subtree groupAsync
  rest <- runTree next sched
  return (tree : rest)

runTree (Free (Describe l subspec next)) sched = do
  status <- liftIO $ newIORef NotStarted
  groupAsync <- liftIO $ async $ runRandomly status
  subtree <- runTree subspec sched
  let tree = RunTreeGroup l status subtree groupAsync
  rest <- runTree next sched
  return (tree : rest)

runTree (Free (DescribeParallel l subspec next)) sched = do
  status <- liftIO $ newIORef NotStarted
  groupAsync <- liftIO $ async $ runRandomly status
  subtree <- runTree subspec sched
  let tree = RunTreeGroup l status subtree groupAsync
  rest <- runTree next sched
  return (tree : rest)

runTree (Free (It l ex next)) sched = do
  RunTreeContext {..} <- ask
  status <- liftIO $ newIORef NotStarted
  groupAsync <- liftIO $ async $ runRandomly status

  -- liftIO $ async $ do
  --   ctx <- wait ctxAsync
  --   scheduleWork sched $ do
  --     startTime <- liftIO getCurrentTime
  --     liftIO $ atomicWriteIORef status (Running startTime)
  --     ret <- ex ctx
  --     atomicWriteIORef status (Done ret)

  let tree = RunTreeSingle l status groupAsync
  rest <- runTree next sched
  return (tree : rest)

runTree (Pure _) sched = return []




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

