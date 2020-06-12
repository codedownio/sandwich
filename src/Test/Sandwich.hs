
module Test.Sandwich where

import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Spec
import Control.Monad.Free
import Test.Sandwich.Interpreters.PrettyShow

pending _ = return $ Result "pending" (Pending Nothing Nothing)

spec :: TopSpec
spec = do
  before "asdf" (\() -> putStrLn "Before") $ do
    it "does the first thing" pending
    it "does the second thing" pending
    it "does the third thing" pending
    describe "nested stuff" $ do
      it "does a nested thing" pending

  introduce "Intro a string" (\() -> getLine >>= \s -> return (s :> ())) $ do
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


pushDownBefores :: Free (SpecCommand context) r -> Free (SpecCommand context) r
pushDownBefores = undefined


main = putStrLn $ prettyShow spec
