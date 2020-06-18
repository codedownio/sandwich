{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.String.Interpolate
import System.Posix.Signals
import Test.Sandwich
import Test.Sandwich.Expectations
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.PrettyShow
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Logging
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.Spec

verySimple :: TopSpec
verySimple = do
  it "succeeds" (return ())
  it "tries shouldBe" (2 `shouldBe` 3)
  it "tries shouldNotBe" (2 `shouldNotBe` 2)
  it "does some logging" $ do
    debug "debug message"
    info "info message"
    warn "warn message"
    logError "error message"

simple :: TopSpec
simple = do
  it "does the thing 1" sleepThenSucceed
  it "does the thing 2" sleepThenSucceed
  it "does the thing 3" sleepThenFail
  it "does the thing 4" sleepThenFail
  it "does the thing 5" sleepThenSucceed
  it "does the thing 6" sleepThenSucceed

medium :: TopSpec
medium = do
  it "does the first thing" sleepThenSucceed
  it "does the 1.5 thing" sleepThenFail
  it "does the 1.8 thing" sleepThenFail

  describe "should happen sequentially" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenFail
    it "sequential 3" sleepThenSucceed

  describeParallel "should happen in parallel" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenSucceed
    it "sequential 3" sleepThenSucceed

  -- around "some around" (\context action -> putStrLn "around1" >> action >> putStrLn "around2") $ do
  --   it "does 1" sleepThenSucceed -- pending
  --   it "does 2" sleepThenSucceed -- pending

  introduce "Database" (\_ -> return (42 :: Int)) (\_ -> return ()) $ do
    it "uses the DB 1" $ do
      num :> _ <- ask
      return ()
      -- liftIO $ putStrLn ("Got num 1: " <> show num)

  --   it "uses the DB 2" $ \((num :: Int) :> ()) -> do
  --     -- putStrLn ("Got num 2: " <> show num)
  --     return Success

  afterEach "after each" (\_ -> return ()) $ do
    beforeEach "before each" (\_ -> return ()) $ do
      it "does the first thing" sleepThenSucceed
      it "does the second thing" sleepThenSucceed
      it "does the third thing" sleepThenSucceed
      describe "nested stuff" $ do
        it "does a nested thing" sleepThenSucceed

  it "does foo" sleepThenFail
  it "does bar" sleepThenSucceed

-- mainFilter :: IO ()
-- mainFilter = putStrLn $ prettyShow $ filterTree "also" topSpec

-- mainPretty :: IO ()
-- mainPretty = putStrLn $ prettyShow topSpec

main :: IO ()
main = runSandwich defaultOptions defaultTerminalUIFormatter medium


-- * Util

sleepThenSucceed :: ExampleM context ()
sleepThenSucceed = do
  liftIO $ threadDelay (2 * 10^6)

sleepThenFail :: ExampleM context ()
sleepThenFail = do
  liftIO $ threadDelay (2 * 10^6)
  2 `shouldBe` 3

pending :: ExampleM context ()
pending = do
  -- failTest Pending
  return ()
