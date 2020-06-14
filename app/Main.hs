module Main where

import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import Control.Scheduler
import System.Posix.Signals
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.PrettyShow
import Test.Sandwich.Interpreters.RunTreeScheduler
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
      putStrLn $ "Got the string: " <> str
      return Success

    it "uses the string again" $ \(str :> ()) -> do
      putStrLn $ "Got the string here: " <> str
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



single :: TopSpec
single = it "does the first thing" pending

double :: TopSpec
double = do
  it "does the first thing" pending
  it "does the second thing" pending

singleWrapped :: TopSpec
singleWrapped = beforeEach "before each" (\() -> putStrLn "before") $ single

doubleWrapped :: TopSpec
doubleWrapped = beforeEach "before each" (\() -> putStrLn "before") $ double
  
mainFilter :: IO ()
mainFilter = putStrLn $ prettyShow $ filterTree "also" topSpec

mainPretty :: IO ()
mainPretty = putStrLn $ prettyShow topSpec

runSandwich :: (Formatter f) => Options -> f -> TopSpec -> IO ()
runSandwich options f spec = do
  withScheduler_ (ParN 2) $ \sched -> do
    asyncUnit <- async $ return ()
    rts <- runReaderT (runTree topSpec) (asyncUnit, sched, options)

    formatterAsync <- async $ runFormatter f rts
  
    let shutdown = do
          putStrLn "TODO: shut down!"
          cancel formatterAsync

    _ <- installHandler sigINT (Catch shutdown) Nothing

    wait formatterAsync


main :: IO ()
main = runSandwich defaultOptions defaultTerminalUIFormatter topSpec
