{-# LANGUAGE DataKinds #-}

module Introduce where


import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Writer
import Data.Foldable
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.STM

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run introduceSyncExceptionDuringAllocate
  run introduceSyncExceptionDuringTest
  run introduceSyncExceptionDuringCleanup
  run introduceAsyncExceptionDuringAllocate
  run introduceAsyncExceptionDuringTest
  run introduceAsyncExceptionDuringCleanup

main :: IO ()
main = mainWith tests

-- * Tests

-- | Sync exception during the allocate phase. Allocation failed, so cleanup does not run
-- and the child fails with a context exception.
introduceSyncExceptionDuringAllocate :: (HasCallStack) => IO ()
introduceSyncExceptionDuringAllocate = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (throwSomeUserError >> return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Failure in introduce 'introduce' allocation handler") someUserErrorWrapped)
                   , Failure (GetContextException Nothing (SomeExceptionWithEq (SomeException (GotException Nothing (Just "Failure in introduce 'introduce' allocation handler") someUserErrorWrapped))))]

-- | Sync exception during the body (the test itself). Allocation succeeded, so cleanup
-- runs; the introduce node succeeds and only the child fails.
introduceSyncExceptionDuringTest :: (HasCallStack) => IO ()
introduceSyncExceptionDuringTest = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" throwSomeUserError

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Success
                   , Failure (GotException Nothing Nothing someUserErrorWrapped)]

-- | Sync exception during the cleanup phase. The body already succeeded; only the
-- introduce node fails.
introduceSyncExceptionDuringCleanup :: (HasCallStack) => IO ()
introduceSyncExceptionDuringCleanup = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (const throwSomeUserError) $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Failure in introduce 'introduce' cleanup handler") someUserErrorWrapped)
                   , Success]

-- | Async exception: cancel the introduce node while the allocate phase is running (before
-- the body). The node is cancelled, cleanup does not run, and the child, which never got a
-- context, fails with a context exception.
introduceAsyncExceptionDuringAllocate :: (HasCallStack) => IO ()
introduceAsyncExceptionDuringAllocate = do
  allocStarted <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduce "introduce" fakeDatabaseLabel (putMVar allocStarted () >> sleepForever >> return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ return ()

  topNode <- case rts of
    [x@(RunNodeIntroduce {runNodeChildrenAugmented=[RunNodeIt {}]})] -> pure x
    _ -> error "Unexpected rts"

  -- Wait until the allocate phase is running, then cancel the top level async
  takeMVar allocStarted
  cancelNode topNode

  -- Waiting for the tree should not throw an exception
  mapM_ waitForTree rts

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree
  let msgs = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   , Failure (GotAsyncException Nothing (Just "introduce introduce alloc handler got async exception") (SomeAsyncExceptionWithEq (SomeAsyncException AsyncCancelled)))]

-- | Async exception: cancel the introduce node while a child test is running. Cleanup runs,
-- and both the introduce node and the child end up cancelled.
introduceAsyncExceptionDuringTest :: (HasCallStack) => IO ()
introduceAsyncExceptionDuringTest = do
  mvar <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ do
      putMVar mvar ()
      sleepForever

  topNode <- case rts of
    [x@(RunNodeIntroduce {runNodeChildrenAugmented=[RunNodeIt {}]})] -> pure x
    _ -> error "Unexpected rts"

  -- Wait until we get into the actual test example, then cancel the top level async
  takeMVar mvar
  cancelNode topNode

  -- Waiting for the tree should not throw an exception
  mapM_ waitForTree rts

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree
  let msgs = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   , Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))]

-- | Async exception: cancel the introduce node while the cleanup phase is running (after the
-- body returned). The body already succeeded; the introduce node is cancelled.
introduceAsyncExceptionDuringCleanup :: (HasCallStack) => IO ()
introduceAsyncExceptionDuringCleanup = do
  cleanupStarted <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> putMVar cleanupStarted () >> sleepForever) $ do
    it "does thing 1" $ return ()

  topNode <- case rts of
    [x@(RunNodeIntroduce {runNodeChildrenAugmented=[RunNodeIt {}]})] -> pure x
    _ -> error "Unexpected rts"

  -- Wait until the cleanup phase is running, then cancel the top level async
  takeMVar cleanupStarted
  cancelNode topNode

  -- Waiting for the tree should not throw an exception
  mapM_ waitForTree rts

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree

  results `mustBe` [Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   , Success]

sleepForever :: MonadUnliftIO m => m ()
sleepForever = threadDelay 999999999999999
