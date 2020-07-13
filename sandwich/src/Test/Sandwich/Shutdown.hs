{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Shutdown where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Test.Sandwich.Types.RunTree


cancelNode :: RunNode context -> IO ()
cancelNode node = readTVarIO (runTreeStatus $ runNodeCommon node) >>= \case
  Running {..} -> cancel statusAsync
  _ -> return ()
