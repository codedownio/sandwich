{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Either
import Data.Foldable
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


main :: (HasCallStack) => IO ()
main = do
  beforeExceptionSafety
  beforeExceptionSafetyNested

  introduceCleansUpOnTestException
  introduceDoesNotCleanUpOnAllocateException
  introduceFailsOnCleanUpException
  introduceCleansUpOnCancelDuringTest

beforeExceptionSafety :: (HasCallStack) => IO ()
beforeExceptionSafety = do
  results <- runAndGetResults $ before "before" throwSomeUserError $ do
    it "does thing 1" $ return ()
    it "does thing 2" $ return ()

  results `mustBe` (Failure (GotException (Just "Exception in before handler") someUserErrorWrapped)
                    : L.replicate 2 (Failure (GetContextException someUserErrorWrapped)))

beforeExceptionSafetyNested :: (HasCallStack) => IO ()
beforeExceptionSafetyNested = do
  results <- runAndGetResults $ before "before" throwSomeUserError $ do
    it "does thing 1" $ return ()
    it "does thing 2" $ return ()
    describe "nested things" $ do
      it "does nested thing 1" $ return ()
      it "does nested thing 2" $ return ()

  results `mustBe` (Failure (GotException (Just "Exception in before handler") someUserErrorWrapped)
                    : L.replicate 5 (Failure (GetContextException someUserErrorWrapped)))

introduceCleansUpOnTestException :: (HasCallStack) => IO ()
introduceCleansUpOnTestException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (debug "doing cleanup") $ do
    it "does thing 1" $ throwSomeUserError

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Success
                   , Failure (GotException (Just "Unknown exception") someUserErrorWrapped)]

introduceDoesNotCleanUpOnAllocateException :: (HasCallStack) => IO ()
introduceDoesNotCleanUpOnAllocateException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (throwSomeUserError >> return FakeDatabase) (debug "doing cleanup") $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException (Just "Exception in allocation handler") someUserErrorWrapped)
                   , Failure (GetContextException (SomeExceptionWithEq (SomeException (GotException (Just "Exception in allocation handler") someUserErrorWrapped))))]

introduceFailsOnCleanUpException :: (HasCallStack) => IO ()
introduceFailsOnCleanUpException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) throwSomeUserError $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException (Just "Exception in cleanup handler") someUserErrorWrapped)
                   , Success]

introduceCleansUpOnCancelDuringTest :: (HasCallStack) => IO ()
introduceCleansUpOnCancelDuringTest = do
  rts <- startSandwichTree defaultOptions $ introduce "introduce" fakeDatabaseLabel (return FakeDatabase) (debug "doing cleanup") $ do
    it "does thing 1" $ liftIO $ threadDelay 999999999999999

  let [RunTreeGroup {runTreeChildren=[RunTreeSingle {runTreeStatus=status, runTreeAsync=theAsync}]}] = rts

  waitUntilRunning status
  cancel theAsync

  eitherResult :: Either SomeException () <- E.try $ mapM_ (wait . runTreeAsync) rts
  isLeft eitherResult `mustBe` True

  fixedTree <- atomically $ mapM fixRunTree rts
  let results = fmap statusToResult $ concatMap getStatuses fixedTree
  let msgs = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree

  msgs `mustBe` [["doing cleanup"], []]
  results `mustBe` [Failure (GotAsyncException Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))
                   ,Failure (GotAsyncException Nothing (SomeAsyncExceptionWithEq $ SomeAsyncException AsyncCancelled))]


-- * Values

data FakeDatabase = FakeDatabase deriving Show
fakeDatabaseLabel = Label :: Label "fakeDatabase" FakeDatabase

someUserError = userError "Oh no"
someUserErrorWrapped = SomeExceptionWithEq $ SomeException $ userError "Oh no"

-- * Helpers

throwSomeUserError :: (MonadIO m) => m ()
throwSomeUserError = liftIO $ throwIO someUserError

runAndGetResults :: (HasCallStack) => TopSpec -> IO [Result]
runAndGetResults spec = do
  finalTree <- runSandwichTree defaultOptions spec
  fixedTree <- atomically $ mapM fixRunTree finalTree
  return $ fmap statusToResult $ concatMap getStatuses fixedTree

runAndGetResultsAndLogs :: TopSpec -> IO ([Result], [[LogStr]])
runAndGetResultsAndLogs spec = do
  finalTree <- runSandwichTree defaultOptions spec
  fixedTree <- atomically $ mapM fixRunTree finalTree
  let results = fmap statusToResult $ concatMap getStatuses fixedTree
  let msgs = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree
  return (results, msgs)

getStatuses :: (HasCallStack) => RunTreeWithStatus a l t -> [a]
getStatuses (RunTreeGroup {..}) = runTreeStatus : (concatMap getStatuses runTreeChildren)
getStatuses (RunTreeSingle {..}) = [runTreeStatus]

getLogs :: (HasCallStack) => RunTreeWithStatus a l t -> [l]
getLogs (RunTreeGroup {..}) = runTreeLogs : (concatMap getLogs runTreeChildren)
getLogs (RunTreeSingle {..}) = [runTreeLogs]

statusToResult :: (HasCallStack) => Status -> Result
statusToResult NotStarted = error "Expected status to be Done but was NotStarted"
statusToResult (Running {}) = error "Expected status to be Done but was Running"
statusToResult (Done _ _ result) = result

mustBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
mustBe x y
  | x == y = return ()
  | otherwise = error [i|Expected #{y} but got #{x}|]

waitUntilRunning status = atomically $ do
  readTVar status >>= \case
    Running {} -> return ()
    _ -> retry
