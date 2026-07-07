
module Test.Sandwich.Shutdown where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Time
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


cancelNode :: RunNode context -> IO ()
cancelNode node = readTVarIO (runTreeStatus $ runNodeCommon node) >>= \case
  Running {..} -> cancel statusAsync
  NotStarted -> do
    now <- getCurrentTime
    forM_ (getCommons node) $ \common ->
      atomically $ modifyTVar' (runTreeStatus common) $ \case
        NotStarted -> Done now Nothing Nothing now Cancelled
        status -> status
  Done {} -> return ()
