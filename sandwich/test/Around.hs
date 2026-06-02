{-# LANGUAGE DataKinds #-}

module Around where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Writer
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import TestUtil
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.STM


tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run aroundDoesNotFailOnChildFailure
  run aroundReceivesSubtreeResult
  run aroundSyncExceptionDuringAllocate
  run aroundSyncExceptionDuringTest
  run aroundSyncExceptionDuringCleanup
  run aroundAsyncExceptionDuringAllocate
  run aroundAsyncExceptionDuringTest
  run aroundAsyncExceptionDuringCleanup

main :: IO ()
main = mainWith tests

-- * Tests

aroundDoesNotFailOnChildFailure :: (HasCallStack) => IO ()
aroundDoesNotFailOnChildFailure = do
  results <- runAndGetResults $ around "around label" void $ do
    it "does thing 1" $ 2 `shouldBe` (3 :: Int)
    it "does thing 1" $ 2 `shouldBe` (2 :: Int)

  case results of
    [Success, Failure {}, Success] -> return ()
    xs -> error [i|Unexpected result: '#{xs}'|]

aroundReceivesSubtreeResult :: (HasCallStack) => IO ()
aroundReceivesSubtreeResult = do
  mvar <- newEmptyMVar

  _ <- runAndGetResults $ around "around label" (>>= (putMVar mvar)) $ do
    it "does thing 1" $ 2 `shouldBe` (3 :: Int)
    it "does thing 1" $ 2 `shouldBe` (2 :: Int)

  takeMVar mvar >>= \case
    [Failure {}, Success] -> return ()
    xs -> error [i|Expected a failure and a success, but got '#{xs}'|]

-- | Sync exception during the allocate phase (the around handler throws before running
-- the children). The around node carries the contextual failure message; the children,
-- which never got a context, fail with a context exception.
aroundSyncExceptionDuringAllocate :: (HasCallStack) => IO ()
aroundSyncExceptionDuringAllocate = do
  results <- runAndGetResults $ around "around label" (\_ -> throwSomeUserError) $ do
    it "does thing 1" $ return ()

  results `mustBe` [Failure (GotException Nothing (Just "Exception in around 'around label' handler") someUserErrorWrapped)
                   , Failure (GetContextException Nothing someUserErrorWrapped)]

-- | Sync exception during the body (the test itself). The around node succeeds; only the
-- child fails.
aroundSyncExceptionDuringTest :: (HasCallStack) => IO ()
aroundSyncExceptionDuringTest = do
  results <- runAndGetResults $ around "around label" void $ do
    it "does thing 1" throwSomeUserError

  results `mustBe` [Success
                   , Failure (GotException Nothing Nothing someUserErrorWrapped)]

-- | Sync exception during the cleanup phase (the around handler throws after running the
-- children). The around node's own status carries the contextual failure message.
aroundSyncExceptionDuringCleanup :: (HasCallStack) => IO ()
aroundSyncExceptionDuringCleanup = do
  results <- runAndGetResults $ around "around label" (\action -> action >> throwSomeUserError) $ do
    it "does thing 1" $ return ()

  results `mustBe` [Failure (GotException Nothing (Just "Exception in around 'around label' handler") someUserErrorWrapped)
                   , Success]

-- | Async exception: cancel the around node while the allocate phase is running (before the
-- children are started). The around node is cancelled and the children, which never got a
-- context, fail with a context exception.
aroundAsyncExceptionDuringAllocate :: (HasCallStack) => IO ()
aroundAsyncExceptionDuringAllocate = do
  allocStarted <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ around "around label" (\_ -> putMVar allocStarted () >> sleepForever) $ do
    it "does thing 1" $ return ()

  topNode <- case rts of
    [x@(RunNodeAround {runNodeChildren=[RunNodeIt {}]})] -> pure x
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

-- | Async exception: cancel the around node while a child test is running.
-- Both the around node and the child should end up cancelled.
aroundAsyncExceptionDuringTest :: (HasCallStack) => IO ()
aroundAsyncExceptionDuringTest = do
  mvar <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ around "around label" void $ do
    it "does thing 1" $ do
      putMVar mvar ()
      sleepForever

  topNode <- case rts of
    [x@(RunNodeAround {runNodeChildren=[RunNodeIt {}]})] -> pure x
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

-- | Async exception: cancel the around node while the cleanup phase is running (after the
-- children returned). The body already succeeded; the around node is cancelled.
aroundAsyncExceptionDuringCleanup :: (HasCallStack) => IO ()
aroundAsyncExceptionDuringCleanup = do
  cleanupStarted <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ around "around label" (\action -> action >> putMVar cleanupStarted () >> sleepForever) $ do
    it "does thing 1" $ return ()

  topNode <- case rts of
    [x@(RunNodeAround {runNodeChildren=[RunNodeIt {}]})] -> pure x
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
