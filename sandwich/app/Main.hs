{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger (LogLevel(..))
import Data.String.Interpolate.IsString
import Data.Time.Clock
import Test.Sandwich
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.TerminalUI

data Database = Database String
  deriving Show

data Foo = Foo { fooInt :: Int, fooString :: String, fooBar :: Bar } deriving (Show, Eq)
data Bar = Bar { barInt :: Int, barString :: String } deriving (Show, Eq)
data Baz = Baz Int String Bar deriving (Show, Eq)
data Simple = Simple { simpleInt :: Int } deriving (Show, Eq)

database = Label :: Label "database" Database
otherDatabase = Label :: Label "otherDatabase" Database


verySimple :: TopSpec
verySimple = do
  it "succeeds" (return ())
  it "tries shouldBe" (2 `shouldBe` 3)
  it "tries shouldBe with Foo" (Foo 2 "asdf" (Bar 2 "asdf") `shouldBe` Foo 3 "fdsa" (Bar 3 "fdsa"))
  it "tries shouldBe with Baz" (Baz 2 "asdf" (Bar 2 "asdf") `shouldBe` Baz 3 "fdsa" (Bar 3 "fdsa"))
  it "tries shouldBe with list" ([1, 2, 3] `shouldBe` [4, 5, 6])
  it "tries shouldBe with tuple" ((1, 2, 3) `shouldBe` (4, 5, 6))
  it "tries shouldBe with list of constructors" ([Simple 1, Simple 2] `shouldBe` [Simple 3, Simple 4])
  it "tries shouldNotBe" (2 `shouldNotBe` 2)
  it "is pending" $ pending
  it "is pending with message" $ pendingWith "Not implemented yet..."
  it "throws an exception" $ do
    2 `shouldBe` 2
    throwIO $ userError "Want a stacktrace here"
    -- 3 `shouldBe` 4
    3 `shouldBe` 3
  it "does some logging" $ do
    debug "debug message"
    info "info message"
    warn "warn message"
    logError "error message"

cancelling :: TopSpec
cancelling = do
  before "succeeds" (debug "before called") $ do
    it "sleeps forever" $ do
      forever $ liftIO $ threadDelay 1
    it "succeeds after 1 second" $ do
      liftIO $ threadDelay 1000000
      return ()

cancellingIntroduce :: TopSpec
cancellingIntroduce = do
  introduce "alloc sleeps forever" database ((forever $ liftIO $ threadDelay 1000000) >> return (Database "foo")) (const $ return ()) $ do
    it "sleeps forever" $ do
      forever $ liftIO $ threadDelay 1
    it "succeeds after 1 second" $ do
      liftIO $ threadDelay 1000000
      return ()

manyRows :: TopSpec
manyRows = do
  forM_ [0..100] $ \n ->
    it [i|does the thing #{n}|] (2 `shouldBe` 2)

simple :: TopSpec
simple = do
  it "does the thing 1" sleepThenSucceed
  it "does the thing 2" sleepThenSucceed
  it "does the thing 3" sleepThenFail
  describe "should happen sequentially" $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenFail
    it "sequential 3" sleepThenSucceed
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

  describe "should happen in parallel" $ parallel $ do
    it "sequential 1" sleepThenSucceed
    it "sequential 2" sleepThenSucceed
    it "sequential 3" sleepThenSucceed

  around "some around" (\action -> debug "around1" >> action >> debug "around2") $ do
    it "does 1" sleepThenSucceed -- pending
    it "does 2" sleepThenSucceed -- pending

  introduceWith "Database around" database (\action -> void $ action (Database "foo")) $ do
    it "uses the DB" $ do
      db <- getContext database
      debug [i|Got db: #{db}|]
      liftIO $ threadDelay (3 * 10^6)

  introduce "Database" database (debug "making DB" >> (return $ Database "outer")) (const $ return ()) $ do
    it "uses the DB 1" $ do
      db <- getContext database
      debug [i|Got db: #{db}|]

    introduce "Database again" database (return $ Database "shadowing") (const $ return ()) $ do
      introduce "Database again" otherDatabase (return $ Database "other") (const $ return ()) $ do
        it "uses the DB 2" $ do
          db <- getContext database
          debug [i|Got db: #{db}|]
          otherDb <- getContext otherDatabase
          debug [i|Got otherDb: #{otherDb}|]

    it "does a thing sequentially" $ sleepThenSucceed
    it "does a thing sequentially 2" $ sleepThenSucceed
    it "does a thing sequentially 3" $ sleepThenSucceed
    it "does a thing sequentially 4" $ sleepThenSucceed

  afterEach "after each" (return ()) $ do
    beforeEach "before each" (return ()) $ do
      it "does the first thing" sleepThenSucceed
      it "does the second thing" sleepThenSucceed
      it "does the third thing" sleepThenSucceed
      describe "nested stuff" $ do
        it "does a nested thing" sleepThenSucceed

  it "does foo" sleepThenFail
  it "does bar" sleepThenSucceed

  after "after" (debug "doing after") $ do
    it "has a thing after it" $ sleepThenSucceed

introduceFailure :: TopSpec
introduceFailure = do
  introduceWith "Database around" database (\action -> liftIO $ throwIO $ userError "Failed to get DB") $ do
    introduce "Database" database (debug "making DB" >> (return $ Database "outer")) (const $ return ()) $ do
      it "uses the DB 1" $ do
        db <- getContext database
        debug [i|Got db: #{db}|]

beforeExceptionSafetyNested :: TopSpec
beforeExceptionSafetyNested = before "before label" (liftIO $ throwIO $ userError "OH NO") $ do
  it "does thing 1" $ return ()
  it "does thing 2" $ return ()
  describe "nested things" $ do
    it "does nested thing 1" $ return ()
    it "does nested thing 2" $ return ()

-- mainFilter :: IO ()
-- mainFilter = putStrLn $ prettyShow $ filterTree "also" topSpec

-- mainPretty :: IO ()
-- mainPretty = putStrLn $ prettyShow topSpec

main :: IO ()
main = runSandwich options medium
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
      , optionsFormatters = [SomeFormatter (defaultTerminalUIFormatter {terminalUILogLevel=(Just LevelWarn), terminalUIInitialFolding=(InitialFoldingTopNOpen 2)})]
      -- , optionsFormatters = [SomeFormatter (defaultPrintFormatter {printFormatterLogLevel=(Just LevelWarn)})]
      }


-- * Util

sleepThenSucceed :: ExampleM context ()
sleepThenSucceed = do
  liftIO $ threadDelay (2 * 10^1)
  -- liftIO $ threadDelay (2 * 10^5)
  -- liftIO $ threadDelay (1 * 10^6)

sleepThenFail :: ExampleM context ()
sleepThenFail = do
  liftIO $ threadDelay (2 * 10^1)
  -- liftIO $ threadDelay (2 * 10^5)
  -- liftIO $ threadDelay (1 * 10^6)
  2 `shouldBe` 3
