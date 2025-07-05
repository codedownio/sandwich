{-# LANGUAGE DataKinds #-}

module Around where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich
import TestUtil
import UnliftIO.Exception


tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run aroundDoesNotFailOnChildFailure
  run aroundReceivesSubtreeResult

main :: IO ()
main = mainWith tests

-- * Tests

aroundDoesNotFailOnChildFailure :: (HasCallStack) => IO ()
aroundDoesNotFailOnChildFailure = do
  results <- runAndGetResults $ around "around label" void $ do
    it "does thing 1" $ 2 `shouldBe` (3 :: Int)
    it "does thing 1" $ 2 `shouldBe` (2 :: Int)

  case results of
    [Success, Failure {}, Success] -> return ()
    xs -> error [i|Unexpected result: '#{xs}'|]

aroundReceivesSubtreeResult :: (HasCallStack) => IO ()
aroundReceivesSubtreeResult = do
  mvar <- newEmptyMVar

  _ <- runAndGetResults $ around "around label" (>>= (liftIO . putMVar mvar)) $ do
    it "does thing 1" $ 2 `shouldBe` (3 :: Int)
    it "does thing 1" $ 2 `shouldBe` (2 :: Int)

  takeMVar mvar >>= \case
    [Failure {}, Success] -> return ()
    xs -> error [i|Expected a failure and a success, but got '#{xs}'|]
