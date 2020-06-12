module Main where

import Test.Sandwich
import Test.Sandwich.Types.Spec
import Test.Sandwich.Interpreters.PrettyShow
import Test.Sandwich.Types.Example

topSpec :: TopSpec
topSpec = do
  before "asdf" (\() -> putStrLn "Before") $ do
    it "does the first thing" pending
    it "does the second thing" pending
    it "does the third thing" pending
    describe "nested stuff" $ do
      it "does a nested thing" pending

  introduce "Intro a string" (\() -> getLine >>= \s -> return (s :> ())) (\_ -> return ()) $ do
    it "uses the string" $ \(str :> ()) -> do
      putStrLn $ "Got the string: " <> str
      return $ Result "" Success

    it "uses the string again" $ \(str :> ()) -> do
      putStrLn $ "Got the string here: " <> str
      return $ Result "" Success

  it "does a thing" $ \() -> do
    putStrLn "HI"
    return $ Result "" Success

  describe "it does this thing also" $ do
    it "does a sub-test" pending

  describeParallel "it does this thing also" $ do
    it "does a sub-test 1" pending
    it "does a sub-test 2" pending
    it "does a sub-test 3" pending

main :: IO ()
main = putStrLn $ prettyShow topSpec
