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


runTree :: (Show r, Show context) => Free (SpecCommand context) r -> ReaderT (Async context, Scheduler IO ()) IO RunningTree

-- runTree indent (Free (Before l subspec next)) =
-- runTree (Free (Introduce l alloc cleanup subspec next)) = do
-- runTree indent (Free (DescribeParallel l subspec next)) =

runTree (Free (It l ex next)) = do
  (ctxAsync, sched) <- ask
  status <- liftIO $ newIORef NotStarted
  liftIO $ async $ do
    ctx <- wait ctxAsync
    scheduleWork sched $ do
      startTime <- liftIO getCurrentTime
      liftIO $ atomicWriteIORef status (Running startTime)
      ret <- ex ctx
      atomicWriteIORef status (Done ret)

  return $ RunningTreeSingle l status

runTree (Free (Describe l subspec next)) = undefined

runTree (Free (DescribeParallel l subspec next)) = undefined

runTree (Pure _) = undefined
