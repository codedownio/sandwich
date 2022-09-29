{-# LANGUAGE DataKinds #-}

module Describe where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich
import TestUtil


tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run describeFailsWhenChildFails

main = mainWith tests

-- * Tests

describeFailsWhenChildFails :: (HasCallStack) => IO ()
describeFailsWhenChildFails = do
  results <- runAndGetResults $ describe "describe label" $ do
    it "does thing 1" $ throwSomeUserError
    it "does thing 2" $ return ()

  (results !! 0) `mustBe` (Failure (ChildrenFailed {failureCallStack = Nothing, failureNumChildren = 1}))
  case results !! 1 of
    Failure (GotException {..}) -> do
      failureException `mustBe` someUserErrorWrapped
    x -> error [i|Expected failure but got: #{x}|]
  (results !! 2) `mustBe` Success
