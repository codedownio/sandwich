{-# LANGUAGE DataKinds #-}

module CancelOnLongExecution (
  tests
  , main
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import UnliftIO.Concurrent
import UnliftIO.Exception

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run cancelOnLongExecutionLeavesNothingUnfinished

main :: IO ()
main = mainWith tests

-- * Tests

-- | With @--cancel-on-long-execution-ms@ the tree gets torn down from several places at once, and
-- a node's async can be killed before it has installed the handler that records its result,
-- leaving the node 'Running' for good.
cancelOnLongExecutionLeavesNothingUnfinished :: (HasCallStack) => IO ()
cancelOnLongExecutionLeavesNothingUnfinished =
  forM_ [1 .. 20 :: Int] $ \iteration -> do
    rts <- runSandwichTree options spec

    waitForAllNodesDone 2_000_000 rts >>= \case
      [] -> return ()
      notDone -> error [i|Iteration #{iteration} left #{length notDone} node(s) unfinished: #{notDone}|]
  where
    options = defaultOptions { optionsCancelOnLongExecutionMs = Just 200 }

-- | A lane pool holds back most of the tests, so nodes are constantly starting when the cancel
-- fires on the wrapper.
spec :: CoreSpec
spec = introduceWith "wrapper" fakeDatabaseLabel (\action -> void $ action FakeDatabase) $
  parallelN 8 $
    forM_ [1 .. 60 :: Int] $ \n ->
      describe [i|group #{n}|] $
        it [i|test #{n}|] $ liftIO $ threadDelay 100_000
