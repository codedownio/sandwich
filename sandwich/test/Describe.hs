{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Describe where


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
-- import Test.Sandwich.Types.Spec

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

  (results !! 0) `mustBe` (Failure (Reason {failureCallStack = Nothing, failureReason = "1 child failed"}))
  case results !! 1 of
    Failure (GotException {..}) -> do
      failureException `mustBe` someUserErrorWrapped
    x -> error [i|Expected failure but got: #{x}|]
  (results !! 2) `mustBe` Success
