-- |

module Test.Sandwich.Interpreters.RunTree (runTree) where

import Control.Monad.Free
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.QSem
import Control.Concurrent.Async
import Control.Exception
import Test.Sandwich.Types.Spec
import Data.IORef
import Test.Sandwich.Types.Example
import Data.Time.Clock
import qualified Data.List as L

data Status = NotStarted
            | Running UTCTime
            | Done Result

type RunningTreeJob = (IORef Status, Async Result)
            
data RunningTree =
  RunningTreeGroup { runningTreeLabel :: String
                   , runningTreeJob :: RunningTreeJob
                   , runningTreeChildren :: [RunningTree]
                   }
  | RunningTreeIntroduce { runningTreeLabel :: String
                         , runningTreeAllocJob :: RunningTreeJob
                         , runningTreeCleanupJob :: RunningTreeJob
                         , runningTreeChildJob :: RunningTreeJob
                         , runningTreeOverallJob :: RunningTreeJob
                         , runningTreeChild :: RunningTree
                         }  
  | RunningTreeItem { runningTreeLabel :: String
                    , runningTreeJob :: RunningTreeJob
                    }

withSemaphore sem = bracket_ (waitQSem sem) (signalQSem sem)

runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (QSem, Async context) IO RunningTree

-- runTree indent (Free (Before l subspec next)) =

runTree (Free (Introduce l alloc cleanup subspec next)) = do
  (sem, ctxAsync) <- ask

  allocStatus <- liftIO $ newIORef NotStarted
  allocJob <- liftIO $ async $ do
    ctx <- wait ctxAsync
    withSemaphore sem $ do
      startTime <- getCurrentTime
      atomicWriteIORef allocStatus (Running startTime)
      ctx' <- alloc ctx
      atomicWriteIORef allocStatus (Done undefined)
      return ctx'

  allocJob2 <- liftIO $ async $ do
    wait allocJob
    return $ Result "" Success

  subtree <- withReaderT (const (sem, allocJob)) $ runTree subspec

  cleanupJob <- undefined
  overallJob <- undefined
  
  return $ RunningTreeIntroduce { runningTreeLabel = l
                                , runningTreeAllocJob = (allocStatus, allocJob2)
                                , runningTreeCleanupJob = cleanupJob
                                , runningTreeChildJob = runningTreeJob subtree
                                , runningTreeOverallJob = overallJob
                                , runningTreeChild = subtree
                                }

-- runTree indent (Free (DescribeParallel l subspec next)) =

runTree (Free (It l ex next)) = do
  (sem, ctxAsync) <- ask
  status <- liftIO $ newIORef NotStarted
  job <- liftIO $ async $ do
    ctx <- wait ctxAsync
    withSemaphore sem $ do
      startTime <- getCurrentTime
      atomicWriteIORef status (Running startTime)
      ret <- ex ctx
      atomicWriteIORef status (Done ret)
      return ret
  return $ RunningTreeItem l (status, job)

runTree (Free (Describe l subspec next)) = undefined

runTree (Free (DescribeParallel l subspec next)) = undefined

runTree (Pure _) = undefined
