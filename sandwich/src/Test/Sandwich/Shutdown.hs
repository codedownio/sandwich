
module Test.Sandwich.Shutdown where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Time
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


cancelNode :: RunNode context -> IO ()
cancelNode node = readTVarIO (runTreeStatus $ runNodeCommon node) >>= \case
  Running {..} -> cancel statusAsync
  NotStarted -> do
    now <- getCurrentTime
    atomically $ writeTVar (runTreeStatus $ runNodeCommon node) (Done now now Cancelled)
  Done {} -> return ()
