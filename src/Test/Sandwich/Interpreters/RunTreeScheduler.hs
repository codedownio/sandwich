{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Interpreters.RunTreeScheduler (runTree) where

import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.QSem
import Control.Concurrent.Async
import Control.Exception
import Test.Sandwich.Types.Spec
import Data.IORef
import Control.Scheduler
import Test.Sandwich.Types.Example
import Data.Time.Clock
import qualified Data.List as L

data Status = NotStarted
            | Running UTCTime
            | Done Result

type RunningTreeStatus = IORef Status

instance Show RunningTreeStatus where
  show _ = "STATUS"

data RunningTree =
  RunningTreeGroupWithStatus { runningTreeLabel :: String
                             , runningTreeStatus :: RunningTreeStatus
                             , runningTreeChildren :: [RunningTree]
                             }
  | RunningTreeGroup { runningTreeLabel :: String
                     , runningTreeChildren :: [RunningTree]
                     }
  | RunningTreeSingle { runningTreeLabel :: String
                      , runningTreeStatus :: RunningTreeStatus
                      }
  deriving Show

runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (Async context, Scheduler IO ()) IO RunningTree

runTree (Free (Before l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  subtree <- runTree subspec
  return $ RunningTreeGroupWithStatus l status [subtree]

runTree (Free (Around l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  let asyncContext = undefined
  (_, sched) <- ask
  subtree <- withReaderT (const (asyncContext, sched)) $ runTree subspec
  return $ RunningTreeGroupWithStatus l status [subtree]

runTree (Free (Introduce l alloc cleanup subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  let asyncContext = undefined
  (_, sched) <- ask
  subtree <- withReaderT (const (asyncContext, sched)) $ runTree subspec
  return $ RunningTreeGroupWithStatus l status [subtree]

runTree (Free (Describe l subspec next)) = do
  subtree <- runTree subspec
  return $ RunningTreeGroup l [subtree]

runTree (Free (DescribeParallel l subspec next)) = do
  subtree <- runTree subspec
  return $ RunningTreeGroup l [subtree]

runTree (Free (It l ex next)) = do
  -- (ctxAsync, sched) <- ask
  status <- liftIO $ newIORef NotStarted
  -- liftIO $ async $ do
  --   ctx <- wait ctxAsync
  --   scheduleWork sched $ do
  --     startTime <- liftIO getCurrentTime
  --     liftIO $ atomicWriteIORef status (Running startTime)
  --     ret <- ex ctx
  --     atomicWriteIORef status (Done ret)

  return $ RunningTreeSingle l status


runTree (Pure _) = undefined
