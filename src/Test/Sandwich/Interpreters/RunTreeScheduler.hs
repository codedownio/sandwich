{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Interpreters.RunTreeScheduler (runTree) where

import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Test.Sandwich.Types.Spec
import Data.IORef
import Control.Scheduler
import Test.Sandwich.Types.Example
import Data.Time.Clock

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

runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (Async context, Scheduler IO ()) IO RunTree

runTree (Free (Before l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  subtree <- runTree subspec
  return $ RunTreeGroupWithStatus l status [subtree]

runTree (Free (Around l f subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  let asyncContext = undefined
  (_, sched) <- ask
  subtree <- withReaderT (const (asyncContext, sched)) $ runTree subspec
  return $ RunTreeGroupWithStatus l status [subtree]

runTree (Free (Introduce l alloc cleanup subspec next)) = do
  status <- liftIO $ newIORef NotStarted
  let asyncContext = undefined
  (_, sched) <- ask
  subtree <- withReaderT (const (asyncContext, sched)) $ runTree subspec
  return $ RunTreeGroupWithStatus l status [subtree]

runTree (Free (Describe l subspec next)) = do
  subtree <- runTree subspec
  return $ RunTreeGroup l [subtree]

runTree (Free (DescribeParallel l subspec next)) = do
  subtree <- runTree subspec
  return $ RunTreeGroup l [subtree]

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

  return $ RunTreeSingle l status


runTree (Pure _) = undefined
