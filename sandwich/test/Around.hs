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
  run aroundReceivesSubtreeResult

main = mainWith tests

-- * Tests

aroundReceivesSubtreeResult :: (HasCallStack) => IO ()
aroundReceivesSubtreeResult = do
  mvar <- newEmptyMVar

  let aroundAction = \action -> do
        result <- action
        liftIO $ putMVar mvar result

  results <- runAndGetResults $ around "around label" aroundAction $ do
    it "does thing 1" $ 2 `shouldBe` 3


  putStrLn [i|Got results: #{results}|]

  val <- takeMVar mvar
  putStrLn [i|Got result: #{val}|]
