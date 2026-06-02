{-# LANGUAGE DataKinds #-}

module IntroduceWith where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Writer
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
  run introduceWithSyncExceptionDuringAllocate
  run introduceWithSyncExceptionDuringTest
  run introduceWithSyncExceptionDuringCleanup
  run introduceWithAsyncExceptionDuringAllocate
  run introduceWithAsyncExceptionDuringTest
  run introduceWithAsyncExceptionDuringCleanup

main :: IO ()
main = mainWith tests

-- * Tests

-- | Sync exception thrown during the allocate phase (before the body action is called).
-- Allocation failed, so the child fails with a context exception.
introduceWithSyncExceptionDuringAllocate :: (HasCallStack) => IO ()
introduceWithSyncExceptionDuringAllocate = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduceWith "introduce with" fakeDatabaseLabel (\_ -> throwSomeUserError) $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Exception in introduceWith 'introduce with' handler") someUserErrorWrapped)
                   , Failure (GetContextException Nothing someUserErrorWrapped)]

-- | Sync exception thrown during the body (the test itself). The introduceWith node
-- succeeds (allocation/cleanup were fine); only the child fails.
introduceWithSyncExceptionDuringTest :: (HasCallStack) => IO ()
introduceWithSyncExceptionDuringTest = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduceWith "introduce with" fakeDatabaseLabel (\action -> void $ action FakeDatabase) $ do
    it "does thing 1" throwSomeUserError

  msgs `mustBe` [[], []]
  results `mustBe` [Success
                   , Failure (GotException Nothing Nothing someUserErrorWrapped)]

-- | Sync exception thrown during the cleanup phase (after the body action returned).
-- The body already succeeded; only the introduceWith node fails.
introduceWithSyncExceptionDuringCleanup :: (HasCallStack) => IO ()
introduceWithSyncExceptionDuringCleanup = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduceWith "introduce with" fakeDatabaseLabel withAction $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Exception in introduceWith 'introduce with' handler") someUserErrorWrapped)
                   , Success]
  where
    withAction action = do
      _ <- action FakeDatabase
      throwSomeUserError

-- | Async exception: cancel the introduceWith node while the allocate phase is running
-- (before the body action is called). The node is cancelled and the child, which never
-- got a context, fails with a context exception.
introduceWithAsyncExceptionDuringAllocate :: (HasCallStack) => IO ()
introduceWithAsyncExceptionDuringAllocate = do
  allocStarted <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduceWith "introduce with" fakeDatabaseLabel (\_ -> putMVar allocStarted () >> sleepForever) $ do
    it "does thing 1" $ return ()

  topNode <- case rts of
    [x@(RunNodeIntroduceWith {runNodeChildrenAugmented=[RunNodeIt {}]})] -> pure x
    _ -> error "Unexpected rts"

  -- Wait until the allocate phase is running, then cancel the top level async
  takeMVar allocStarted
  cancelNode topNode

  -- Waiting for the tree should not throw an exception
  mapM_ waitForTree rts

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree

  results `mustBe` [Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   , Failure (GetContextException Nothing (SomeExceptionWithEq (toException (SomeAsyncException AsyncCancelled))))]

-- | Async exception: cancel the introduceWith node while a child test is running.
-- Both the introduceWith node and the child should end up cancelled.
introduceWithAsyncExceptionDuringTest :: (HasCallStack) => IO ()
introduceWithAsyncExceptionDuringTest = do
  mvar <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduceWith "introduce with" fakeDatabaseLabel (\action -> void $ action FakeDatabase) $ do
    it "does thing 1" $ do
      putMVar mvar ()
      sleepForever

  topNode <- case rts of
    [x@(RunNodeIntroduceWith {runNodeChildrenAugmented=[RunNodeIt {}]})] -> pure x
    _ -> error "Unexpected rts"

  -- Wait until we get into the actual test example, then cancel the top level async
  takeMVar mvar
  cancelNode topNode

  -- Waiting for the tree should not throw an exception
  mapM_ waitForTree rts

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree

  results `mustBe` [Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   , Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))]

-- | Async exception: cancel the introduceWith node while the cleanup phase is running
-- (after the body action returned). The body already succeeded; the node is cancelled.
introduceWithAsyncExceptionDuringCleanup :: (HasCallStack) => IO ()
introduceWithAsyncExceptionDuringCleanup = do
  cleanupStarted <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduceWith "introduce with" fakeDatabaseLabel (\action -> void (action FakeDatabase) >> putMVar cleanupStarted () >> sleepForever) $ do
    it "does thing 1" $ return ()

  topNode <- case rts of
    [x@(RunNodeIntroduceWith {runNodeChildrenAugmented=[RunNodeIt {}]})] -> pure x
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
