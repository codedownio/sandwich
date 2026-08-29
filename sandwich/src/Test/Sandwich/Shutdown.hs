
module Test.Sandwich.Shutdown where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


cancelNode :: RunNode context -> IO ()
cancelNode node = do
  readTVarIO (runTreeStatus $ runNodeCommon node) >>= \case
    Running {..} -> cancel statusAsync
    _ -> return ()

  -- 'cancel' doesn't return until the async has finished, and a node that hasn't started under
  -- a cancelled parent never will, so anything in here that isn't 'Done' by now never will be.
  markUnfinishedNodesDone node Cancelled
