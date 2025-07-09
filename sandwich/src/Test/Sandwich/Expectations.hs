{-# LANGUAGE RankNTypes #-}

-- | Functions for making assertions about test behavior.

module Test.Sandwich.Expectations where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.List as L
import qualified Data.Set as Set
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Test.Sandwich.Types.Spec
import UnliftIO.Exception

-- * Manually fail a test or mark as pending

-- | General-purpose function to throw a test exception with a 'String'.
expectationFailure :: (HasCallStack, MonadIO m) => String -> m a
expectationFailure = throwIO . Reason (Just callStack)

-- | Throws a 'Pending' exception, which will cause the test to be marked as pending.
pending :: (HasCallStack, MonadIO m) => m a
pending = throwIO $ Pending (Just callStack) Nothing

-- | Throws a 'Pending' exception with a message to add additional details.
pendingWith :: (HasCallStack, MonadIO m) => String -> m a
pendingWith msg = throwIO $ Pending (Just callStack) (Just msg)

-- | Shorthand for a pending test example. You can quickly mark an 'it' node as pending by putting an "x" in front of it.
xit :: (HasCallStack, MonadIO m) => String -> ExampleT context m () -> SpecFree context m ()
xit name _ex = it name (throwIO $ Pending (Just callStack) Nothing)

-- * Expecting failures

-- | Assert that a given action should fail with some 'FailureReason'.
shouldFail :: (HasCallStack, MonadUnliftIO m) => m () -> m ()
shouldFail action = do
  try action >>= \case
    Left (_ :: FailureReason) -> return ()
    Right () -> expectationFailure [i|Expected test to fail|]

-- | Assert that a given action should fail with some 'FailureReason' matching a predicate.
shouldFailPredicate :: (HasCallStack, MonadUnliftIO m) => (FailureReason -> Bool) -> m () -> m ()
shouldFailPredicate p action = do
  try action >>= \case
    Left (err :: FailureReason) -> case p err of
      True -> return ()
      False -> expectationFailure [i|Expected test to fail with a failure matching the predicate, but got a different failure: '#{err}'|]
    Right () -> expectationFailure [i|Expected test to fail, but it succeeded|]

-- | Asserts that an action should throw an exception. Accepts a predicate to determine if the exception matches.
shouldThrow :: (HasCallStack, MonadUnliftIO m, Exception e) =>
  m a
  -- ^ The action to run.
  -> (e -> Bool)
  -- ^ A predicate on the exception to determine if it's as expected.
  -> m ()
shouldThrow action f = do
  try action >>= \case
    Right _ -> expectationFailure [i|Expected exception to be thrown.|]
    Left e | f e -> return ()
    Left e -> expectationFailure [i|Exception didn't match predicate: '#{show e}'|]

-- * Assertions

-- | Asserts that two things are equal.
shouldBe :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
shouldBe x y
  | x == y = return ()
  | otherwise = throwIO (ExpectedButGot (Just callStack) (SEB y) (SEB x))

-- | Asserts that two things are not equal.
shouldNotBe :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
shouldNotBe x y
  | x /= y = return ()
  | otherwise = throwIO (DidNotExpectButGot (Just callStack) (SEB y))

-- | Asserts that the given list contains a subsequence.
shouldContain :: (HasCallStack, MonadIO m, Eq a, Show a) => [a] -> [a] -> m ()
shouldContain haystack needle = case needle `L.isInfixOf` haystack of
  True -> return ()
  False -> expectationFailure [i|Expected #{show haystack} to contain #{show needle}|] -- TODO: custom exception type

-- | Asserts that the given list contains an item matching a predicate.
shouldContainPredicate :: (HasCallStack, MonadIO m, Show a) => [a] -> (a -> Bool) -> m ()
shouldContainPredicate haystack p = case L.find p haystack of
  Just _ -> return ()
  Nothing -> expectationFailure [i|Expected #{show haystack} to contain an item matching the predicate|]

-- | Asserts that the given list does not contain a subsequence.
shouldNotContain :: (HasCallStack, MonadIO m, Eq a, Show a) => [a] -> [a] -> m ()
shouldNotContain haystack needle = case needle `L.isInfixOf` haystack of
  True -> expectationFailure [i|Expected #{show haystack} not to contain #{show needle}|]
  False -> return ()

-- | Asserts that the given lists are equal as sets.
shouldBeSet :: (HasCallStack, MonadIO m, Ord a, Show a) => [a] -> [a] -> m ()
shouldBeSet haystack needle = case Set.fromList needle == Set.fromList haystack of
  True -> return ()
  False -> expectationFailure [i|Expected #{show haystack} to equal as a set #{show needle}|]

-- | Asserts that the given list contains an item matching a predicate.
shouldNotContainPredicate :: (HasCallStack, MonadIO m, Show a) => [a] -> (a -> Bool) -> m ()
shouldNotContainPredicate haystack p = case L.find p haystack of
  Nothing -> return ()
  Just _ -> expectationFailure [i|Expected #{show haystack} not to contain an item matching the predicate|]

-- | Asserts that the given 'Maybe' is 'Nothing'.
shouldBeNothing :: (HasCallStack, MonadIO m, Show a) => Maybe a -> m ()
shouldBeNothing Nothing = return ()
shouldBeNothing x = expectationFailure [i|Expected Nothing but got #{x}|]

-- | Asserts that the given 'Maybe' is 'Just'.
shouldBeJust :: (HasCallStack, MonadIO m) => Maybe a -> m ()
shouldBeJust (Just _) = return ()
shouldBeJust Nothing = expectationFailure [i|Expected Just but got Nothing.|]

-- | Asserts that the given 'Either' is 'Left'.
shouldBeLeft :: (HasCallStack, MonadIO m, Show a, Show b) => Either a b -> m ()
shouldBeLeft (Left _) = return ()
shouldBeLeft x = expectationFailure [i|Expected Left but got #{x}|]

-- | Asserts that the given 'Either' is 'Right'.
shouldBeRight :: (HasCallStack, MonadIO m, Show a, Show b) => Either a b -> m ()
shouldBeRight (Right _) = return ()
shouldBeRight x = expectationFailure [i|Expected Right but got #{x}.|]

-- | Asserts that the given text contains a substring.
textShouldContain :: (HasCallStack, MonadIO m) => T.Text -> T.Text -> m ()
t `textShouldContain` txt = ((T.unpack t) :: String) `shouldContain` (T.unpack txt)

-- | Asserts that the given text does not contain a substring.
textShouldNotContain :: (HasCallStack, MonadIO m) => T.Text -> T.Text -> m ()
t `textShouldNotContain` txt = ((T.unpack t) :: String) `shouldNotContain` (T.unpack txt)
