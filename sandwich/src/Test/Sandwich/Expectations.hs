{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Test.Sandwich.Expectations where

import Control.Monad.Except
import Control.Monad.Free.Class
import qualified Data.Text as T
import GHC.Stack
import Test.Sandwich.Types.Spec

-- * Manually fail a test or mark as pending

expectationFailure :: (HasCallStack, MonadError FailureReason m) => String -> m ()
expectationFailure = throwError . Reason (Just callStack)

pending :: (HasCallStack, MonadError FailureReason m) => m ()
pending = throwError $ Pending (Just callStack) Nothing

pendingWith :: (HasCallStack, MonadError FailureReason m) => String -> m ()
pendingWith msg = throwError $ Pending (Just callStack) (Just msg)

xit :: (HasCallStack, Monad m) => String -> ExampleT context m1 () -> SpecFree context m ()
xit name _ex = it name (throwError $ Pending (Just callStack) Nothing)

-- * Assertions

shouldBe :: (HasCallStack, MonadError FailureReason m, Eq a, Show a) => a -> a -> m ()
shouldBe x y
  | x == y = return ()
  | otherwise = throwError (ExpectedButGot (Just callStack) (SEB x) (SEB y))

shouldNotBe :: (HasCallStack, MonadError FailureReason m, Eq a, Show a) => a -> a -> m ()
shouldNotBe x y
  | x /= y = return ()
  | otherwise = throwError (DidNotExpectButGot (Just callStack) (SEB y))

shouldContain :: (HasCallStack, MonadError FailureReason m, Eq a, Show a) => [a] -> [a] -> m ()
shouldContain = undefined

shouldNotContain :: (HasCallStack, MonadError FailureReason m, Eq a, Show a) => [a] -> [a] -> m ()
shouldNotContain = undefined

-- | Asserts that the given text contains a substring.
textShouldContain :: (HasCallStack, MonadError FailureReason m) => T.Text -> T.Text -> m ()
t `textShouldContain` txt = ((T.unpack t) :: String) `shouldContain` (T.unpack txt)

-- | Asserts that the given text does not contain a substring.
textShouldNotContain :: (HasCallStack, MonadError FailureReason m) => T.Text -> T.Text -> m ()
t `textShouldNotContain` txt = ((T.unpack t) :: String) `shouldNotContain` (T.unpack txt)
