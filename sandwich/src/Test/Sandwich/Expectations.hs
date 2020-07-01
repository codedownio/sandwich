{-# LANGUAGE FlexibleContexts #-}
-- |

module Test.Sandwich.Expectations where

import Control.Monad.Except
import GHC.Stack
import Test.Sandwich.Types.Spec

expectationFailure :: (HasCallStack, MonadError FailureReason m) => String -> m ()
expectationFailure = throwError . Reason (Just callStack)

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
