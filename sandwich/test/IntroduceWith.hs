{-# LANGUAGE DataKinds #-}

module IntroduceWith where


import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import GHC.Stack
import Test.Sandwich
import UnliftIO.Exception

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run introduceWithFailsOnAllocateException
  run introduceWithFailsOnCleanUpException

main :: IO ()
main = mainWith tests

-- * Tests

introduceWithFailsOnAllocateException :: (HasCallStack) => IO ()
introduceWithFailsOnAllocateException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduceWith "introduce with" fakeDatabaseLabel withAction $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Exception in introduceWith 'introduce with' handler") someUserErrorWrapped)
                   , Failure (GetContextException Nothing someUserErrorWrapped)]
  where
    withAction action = do
      throwSomeUserError
      _ <- action FakeDatabase
      return ()

introduceWithFailsOnCleanUpException :: (HasCallStack) => IO ()
introduceWithFailsOnCleanUpException = do
  (results, msgs) <- runAndGetResultsAndLogs $ introduceWith "introduce with" fakeDatabaseLabel withAction $ do
    it "does thing 1" $ return ()

  msgs `mustBe` [[], []]
  results `mustBe` [Failure (GotException Nothing (Just "Exception in introduceWith 'introduce with' handler") someUserErrorWrapped)
                   , Success]
  where
    withAction action = do
      _ <- action FakeDatabase
      throwSomeUserError
