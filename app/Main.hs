{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad.Trans.Reader
import Data.String.Interpolate
import System.Posix.Signals
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.PrettyShow
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.Spec


topSpec :: TopSpec
topSpec = do
  afterEach "after each" (\() -> putStrLn "after") $ do
    beforeEach "before each" (\() -> putStrLn "before") $ do
      it "does the first thing" pending
      it "does the second thing" pending
      it "does the third thing" pending
      describe "nested stuff" $ do
        it "does a nested thing" pending

  around "some around" (\context action -> putStrLn "around1" >> action >> putStrLn "around2") $ do
    it "does 1" pending
    it "does 2" pending

  introduce "Intro a string" (\() -> getLine) (\_ -> return ()) $ do
    it "uses the string" $ \(str :> ()) -> do
      -- putStrLn $ "Got the string: " <> str
      return Success

    it "uses the string again" $ \(str :> ()) -> do
      -- putStrLn $ "Got the string here: " <> str
      return Success

  it "does a thing" $ \() -> do
    putStrLn "HI"
    return Success

  describe "it does this thing also" $ do
    it "does a sub-test" pending

  describeParallel "it does this thing also" $ do
    it "does a first sub-test 1" pending
    it "does a sub-test 2" pending
    it "does a sub-test 3" pending



sleepThenSucceed _ = do
  threadDelay (2 * 10^6)
  return Success

sleepThenFail _ = do
  threadDelay (2 * 10^6)
  return $ Failure Nothing (ExpectedButGot "2" "3")

verySimple :: TopSpec
verySimple = do
  it "does the only thing" (\_ -> threadDelay (15 * 10^6) >> return Success)

simple :: TopSpec
simple = do
  it "does the thing 1" sleepThenSucceed
  it "does the thing 2" sleepThenSucceed
  it "does the thing 3" sleepThenSucceed
  it "does the thing 4" sleepThenSucceed
  it "does the thing 5" sleepThenSucceed
  it "does the thing 6" sleepThenSucceed

medium :: TopSpec
medium = do
  it "does the first thing" sleepThenSucceed
  it "does the 1.5 thing" sleepThenFail
  it "does the 1.8 thing" sleepThenFail

  describe "should happen sequentially" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenSucceed
    it "sequential 3" sleepThenSucceed

  describeParallel "should happen in parallel" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenSucceed
    it "sequential 3" sleepThenSucceed

  introduce "Database" (\() -> return 42) (\(num :> ()) -> return ()) $ do
    it "uses the DB 1" $ \(num :> ()) -> do
      -- putStrLn ("Got num 1: " <> show num)
      return Success

    it "uses the DB 2" $ \(num :> ()) -> do
      -- putStrLn ("Got num 2: " <> show num)
      return Success

  it "does foo" sleepThenFail
  it "does bar" sleepThenSucceed

mainFilter :: IO ()
mainFilter = putStrLn $ prettyShow $ filterTree "also" topSpec

mainPretty :: IO ()
mainPretty = putStrLn $ prettyShow topSpec

main :: IO ()
main = runSandwich defaultOptions defaultTerminalUIFormatter medium
