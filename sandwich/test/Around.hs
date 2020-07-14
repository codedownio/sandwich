{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Around where


import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Sandwich

import TestUtil

tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run aroundDoesNotFailureOnChildFailure
  run aroundReceivesSubtreeResult

main = mainWith tests

-- * Tests


aroundDoesNotFailureOnChildFailure :: (HasCallStack) => IO ()
aroundDoesNotFailureOnChildFailure = do
  results <- runAndGetResults $ around "around label" void $ do
    it "does thing 1" $ 2 `shouldBe` 3
    it "does thing 1" $ 2 `shouldBe` 2

  case results of
    [Success, Failure {}, Success] -> return ()
    xs -> error [i|Unexpected result: '#{xs}'|]

aroundReceivesSubtreeResult :: (HasCallStack) => IO ()
aroundReceivesSubtreeResult = do
  mvar <- newEmptyMVar

  _ <- runAndGetResults $ around "around label" (>>= (liftIO . putMVar mvar)) $ do
    it "does thing 1" $ 2 `shouldBe` 3
    it "does thing 1" $ 2 `shouldBe` 2

  takeMVar mvar >>= \case
    [Failure {}, Success] -> return ()
    xs -> error [i|Expected a failure and a success, but got '#{xs}'|]
