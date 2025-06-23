{-# LANGUAGE DataKinds #-}

module Before where

import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import qualified Data.List as L
import GHC.Stack
import Test.Sandwich
import TestUtil
import UnliftIO.Exception


tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run beforeExceptionSafety
  run beforeExceptionSafetyNested

main :: IO ()
main = mainWith tests

-- * Tests

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
