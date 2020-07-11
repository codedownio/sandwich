{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Before where


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

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run beforeExceptionSafety
  run beforeExceptionSafetyNested

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
