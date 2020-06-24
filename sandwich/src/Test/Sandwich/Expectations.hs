-- |

module Test.Sandwich.Expectations where

import Control.Monad.Except
import GHC.Stack
import Test.Sandwich.Types.Spec
import qualified Text.Show.Pretty as P

expectationFailure :: (HasCallStack) => String -> ExampleM context ()
expectationFailure = throwError . Reason (Just callStack)

shouldBe :: (HasCallStack, Eq a, Show a) => a -> a -> ExampleM context ()
shouldBe x y
  | x == y = return ()
  | otherwise = case (P.reify x, P.reify y) of
      (Just vx, Just vy) -> throwError (ExpectedButGotValue (Just callStack) vx vy)
      _ -> throwError (ExpectedButGot (Just callStack) (show x) (show y))

shouldNotBe :: (HasCallStack, Eq a, Show a) => a -> a -> ExampleM context ()
shouldNotBe x y
  | x /= y = return ()
  | otherwise = case (P.reify x, P.reify y) of
      (Just vx, Just vy) -> throwError (DidNotExpectButGotValue (Just callStack) vy)
      _ -> throwError (DidNotExpectButGot (Just callStack) (show y))
