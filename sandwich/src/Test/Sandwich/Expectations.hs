-- |

module Test.Sandwich.Expectations where

import Control.Monad.Except
import GHC.Stack
import Test.Sandwich.Types.Spec

expectationFailure :: (HasCallStack) => String -> ExampleM context ()
expectationFailure = throwError . Reason (Just callStack)

shouldBe :: (HasCallStack, Eq a, Show a) => a -> a -> ExampleM context ()
shouldBe x y
  | x == y = return ()
  | otherwise = throwError (ExpectedButGot (Just callStack) (show x) (show y))

shouldNotBe :: (HasCallStack, Eq a, Show a) => a -> a -> ExampleM context ()
shouldNotBe x y
  | x /= y = return ()
  | otherwise = throwError (DidNotExpectButGot (Just callStack) (show y))
