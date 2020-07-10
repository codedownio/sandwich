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
import Control.Monad.Trans.Writer
import Data.Either
import Data.Foldable
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import System.Exit
import Test.Sandwich
import Test.Sandwich.Internal


main :: (HasCallStack) => IO ()
main = do
  results <- execWriterT $ do
    run beforeExceptionSafety
    run beforeExceptionSafetyNested

    run introduceCleansUpOnTestException
    run introduceDoesNotCleanUpOnAllocateException
    run introduceFailsOnCleanUpException
    run introduceCleansUpOnCancelDuringTest

  case results of
    [] -> return ()
    xs -> do
      putStrLn [i|\n\n#{length xs} test(s) failed\n\n|]
      forM_ xs $ \x -> putStrLn [i|#{x}\n\n|]
      exitWith (ExitFailure 1)

beforeExceptionSafety :: (HasCallStack) => IO ()
beforeExceptionSafety = do
  results <- runAndGetResults $ before "before label" throwSomeUserError $ do
    it "does thing 1" $ return ()
    it "does thing 2" $ return ()

  results `mustBe` (Failure (GotException Nothing (Just "Exception in before 'before label' handler") someUserErrorWrapped)
                    : L.replicate 2 (Failure (GetContextException Nothing (SomeExceptionWithEq (toException $ GotException Nothing (Just "Exception in before 'before label' handler") someUserErrorWrapped)))))

beforeExceptionSafetyNested :: (HasCallStack) => IO ()
beforeExceptionSafetyNested = do
  results <- runAndGetResults $ before "before label" throwSomeUserError $ do
    it "does thing 1" $ return ()
    it "does thing 2" $ return ()
    describe "nested things" $ do
      it "does nested thing 1" $ return ()
      it "does nested thing 2" $ return ()

  results `mustBe` (Failure (GotException Nothing (Just "Exception in before 'before label' handler") someUserErrorWrapped)
                    : L.replicate 5 (Failure (GetContextException Nothing (SomeExceptionWithEq (toException $ GotException Nothing (Just "Exception in before 'before label' handler") someUserErrorWrapped)))))

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


-- * Values

data FakeDatabase = FakeDatabase deriving Show
fakeDatabaseLabel = Label :: Label "fakeDatabase" FakeDatabase

someUserError = userError "Oh no"
someUserErrorWrapped = SomeExceptionWithEq $ SomeException $ userError "Oh no"

-- * Helpers

run test = (liftIO $ tryAny test) >>= \case
  Left err -> tell [err]
  Right () -> return ()

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
  getResultsAndMessages <$> fixTree finalTree

fixTree rts = atomically $ mapM fixRunTree rts

getResultsAndMessages fixedTree = (results, msgs)
  where
    results = fmap statusToResult $ concatMap getStatuses fixedTree
    msgs = getMessages fixedTree

getMessages fixedTree = fmap (toList . (fmap logEntryStr)) $ concatMap getLogs fixedTree

getStatuses :: (HasCallStack) => RunTreeWithStatus a l t -> [(String, a)]
getStatuses (RunTreeGroup {..}) = (runTreeLabel, runTreeStatus) : (concatMap getStatuses runTreeChildren)
getStatuses (RunTreeSingle {..}) = [(runTreeLabel, runTreeStatus)]

getLogs :: (HasCallStack) => RunTreeWithStatus a l t -> [l]
getLogs (RunTreeGroup {..}) = runTreeLogs : (concatMap getLogs runTreeChildren)
getLogs (RunTreeSingle {..}) = [runTreeLogs]

statusToResult :: (HasCallStack) => (String, Status) -> Result
statusToResult (label, NotStarted) = error [i|Expected status to be Done but was NotStarted for label '#{label}'|]
statusToResult (label, Running {}) = error [i|Expected status to be Done but was Running for label '#{label}'|]
statusToResult (_, Done _ _ result) = result

mustBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
mustBe x y
  | x == y = return ()
  | otherwise = error [i|Expected #{y} but got #{x}|]

waitUntilRunning status = atomically $ do
  readTVar status >>= \case
    Running {} -> return ()
    _ -> retry

printFixedRunTree :: RunTreeFixed -> IO ()
printFixedRunTree = printFixedRunTree' 0
  where
    printFixedRunTree' :: Int -> RunTreeFixed -> IO ()
    printFixedRunTree' indent (RunTreeGroup {..}) = do
      putStrLn [i|#{indentation}#{runTreeLabel} [#{statusResult runTreeStatus}]|]
      forM_ runTreeChildren (printFixedRunTree' (indent + 1))
      where indentation = L.replicate (indent * 4) ' '
    printFixedRunTree' indent (RunTreeSingle {..}) = putStrLn [i|#{indentation}#{runTreeLabel} [#{statusResult runTreeStatus}]|]
      where indentation = L.replicate (indent * 4) ' '
