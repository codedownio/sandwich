{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Expectations where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import Test.Sandwich.Types.Spec

-- * Manually fail a test or mark as pending

expectationFailure :: (HasCallStack, MonadThrow m) => String -> m ()
expectationFailure = throwIO . Reason (Just callStack)

pending :: (HasCallStack, MonadThrow m) => m ()
pending = throwIO $ Pending (Just callStack) Nothing

pendingWith :: (HasCallStack, MonadThrow m) => String -> m ()
pendingWith msg = throwIO $ Pending (Just callStack) (Just msg)

xit :: (HasCallStack, Monad m, MonadThrow m) => String -> ExampleT context m1 () -> SpecFree context m ()
xit name _ex = it name (throwIO $ Pending (Just callStack) Nothing)

-- * Assertions

-- | Asserts that two things are equal.
shouldBe :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> a -> m ()
shouldBe x y
  | x == y = return ()
  | otherwise = throwIO (ExpectedButGot (Just callStack) (SEB y) (SEB x))

-- | Asserts that two things are not equal.
shouldNotBe :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> a -> m ()
shouldNotBe x y
  | x /= y = return ()
  | otherwise = throwIO (DidNotExpectButGot (Just callStack) (SEB y))

-- | Asserts that the given list contains a subsequence.
shouldContain :: (HasCallStack, MonadThrow m, Eq a, Show a) => [a] -> [a] -> m ()
shouldContain haystack needle = case needle `L.isInfixOf` haystack of
  True -> return ()
  False -> expectationFailure [i|Expected #{show haystack} to contain #{show needle}|] -- TODO: custom exception type

-- | Asserts that the given list does not contain a subsequence.
shouldNotContain :: (HasCallStack, MonadThrow m, Eq a, Show a) => [a] -> [a] -> m ()
shouldNotContain haystack needle = case needle `L.isInfixOf` haystack of
  True -> expectationFailure [i|Expected #{show haystack} not to contain #{show needle}|]
  False -> return ()

-- | Asserts that the given text contains a substring.
textShouldContain :: (HasCallStack, MonadThrow m) => T.Text -> T.Text -> m ()
t `textShouldContain` txt = ((T.unpack t) :: String) `shouldContain` (T.unpack txt)

-- | Asserts that the given text does not contain a substring.
textShouldNotContain :: (HasCallStack, MonadThrow m) => T.Text -> T.Text -> m ()
t `textShouldNotContain` txt = ((T.unpack t) :: String) `shouldNotContain` (T.unpack txt)

-- | Asserts that an IO action should throw an exception.
shouldThrow :: (HasCallStack, MonadThrow m, MonadIO m, Exception e) =>
  IO a
  -- ^ The action to run.
  -> (e -> Bool)
  -- ^ A predicate on the exception to determine if it's as expected.
  -> m ()
shouldThrow action f = do
  liftIO (try action) >>= \case
    Right _ -> expectationFailure [i|Expected exception to be thrown.|]
    Left e | f e -> return ()
    Left e -> expectationFailure [i|Predicate failed on expected exception: '#{e}'|]
