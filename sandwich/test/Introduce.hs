{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Introduce where


import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Writer
import Data.Either
import Data.Foldable
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import System.Exit
import Test.Sandwich
import Test.Sandwich.Internal

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run introduceCleansUpOnTestException
  run introduceDoesNotCleanUpOnAllocateException
  run introduceFailsOnCleanUpException
  run introduceCleansUpOnCancelDuringTest

main = mainWith tests

-- * Tests

introduceCleansUpOnTestException :: (HasCallStack) => IO ()
introduceCleansUpOnTestException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ throwSomeUserError

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Success
                   , Failure (GotException Nothing Nothing someUserErrorWrapped)]

introduceDoesNotCleanUpOnAllocateException :: (HasCallStack) => IO ()
introduceDoesNotCleanUpOnAllocateException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (throwSomeUserError >> return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Exception in allocation 'introduce' handler") someUserErrorWrapped)
                   , Failure (GetContextException Nothing (SomeExceptionWithEq (SomeException (GotException Nothing (Just "Exception in allocation 'introduce' handler") someUserErrorWrapped))))]

introduceFailsOnCleanUpException :: (HasCallStack) => IO ()
introduceFailsOnCleanUpException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> throwSomeUserError) $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Exception in cleanup 'introduce' handler") someUserErrorWrapped)
                   , Success]

introduceCleansUpOnCancelDuringTest :: (HasCallStack) => IO ()
introduceCleansUpOnCancelDuringTest = do
  mvar <- newEmptyMVar

  rts <- startSandwichTree defaultOptions $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (\_ -> debug "doing cleanup") $ do
    it "does thing 1" $ do
      liftIO $ putMVar mvar ()
      liftIO $ threadDelay 999999999999999

  let [RunTreeGroup {runTreeChildren=[RunTreeSingle {runTreeStatus=status, runTreeAsync=theAsync}]}] = rts

  -- Wait until we get into the actual test example, then cancel the top level async
  takeMVar mvar
  cancel theAsync

  -- We should get an async exception
  eitherResult :: Either SomeException (Either [SomeException] ()) <- E.try $ waitForTree rts
  isLeft eitherResult `mustBe` True

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree
  let msgs = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   , Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))]
