{-# LANGUAGE DataKinds #-}

module Introduce where


import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Internal
import UnliftIO.Exception

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run introduceCleansUpOnTestException
  run introduceDoesNotCleanUpOnAllocateException
  run introduceFailsOnCleanUpException
  run introduceCleansUpOnCancelDuringTest

main :: IO ()
main = mainWith tests

-- * Tests

introduceCleansUpOnTestException :: (HasCallStack) => IO ()
introduceCleansUpOnTestException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" throwSomeUserError

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Success
                   , Failure (GotException Nothing Nothing someUserErrorWrapped)]

introduceDoesNotCleanUpOnAllocateException :: (HasCallStack) => IO ()
introduceDoesNotCleanUpOnAllocateException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (throwSomeUserError >> return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Failure in introduce 'introduce' allocation handler") someUserErrorWrapped)
                   , Failure (GetContextException Nothing (SomeExceptionWithEq (SomeException (GotException Nothing (Just "Failure in introduce 'introduce' allocation handler") someUserErrorWrapped))))]

introduceFailsOnCleanUpException :: (HasCallStack) => IO ()
introduceFailsOnCleanUpException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (const throwSomeUserError) $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Failure in introduce 'introduce' cleanup handler") someUserErrorWrapped)
                   , Success]

introduceCleansUpOnCancelDuringTest :: (HasCallStack) => IO ()
introduceCleansUpOnCancelDuringTest = do
  mvar <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ do
      liftIO $ putMVar mvar ()
      liftIO $ threadDelay 999999999999999

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
