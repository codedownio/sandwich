{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Concurrent.STM
import Control.Exception.Safe
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.IO.Exception
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


main :: (HasCallStack) => IO ()
main = do
  beforeExceptionSafety
  beforeExceptionSafetyNested



beforeExceptionSafety :: (HasCallStack) => IO ()
beforeExceptionSafety = do
  results <- runAndGetResults $ before "before" (\_ -> throwIO someUserError) $ do
    it "does thing 1" $ return ()
    it "does thing 2" $ return ()

  results `mustBe` (Failure (GotException (Just "Exception in before handler") someUserErrorWrapped)
                    : L.replicate 2 (Failure (GetContextException someUserErrorWrapped)))

beforeExceptionSafetyNested :: (HasCallStack) => IO ()
beforeExceptionSafetyNested = do
  results <- runAndGetResults $ before "before" (\_ -> throwIO someUserError) $ do
    it "does thing 1" $ return ()
    it "does thing 2" $ return ()
    describe "nested things" $ do
      it "does nested thing 1" $ return ()
      it "does nested thing 2" $ return ()


  results `mustBe` (Failure (GotException (Just "Exception in before handler") someUserErrorWrapped)
                    : L.replicate 5 (Failure (GetContextException someUserErrorWrapped)))

-- * Values

someUserError = userError "Oh no"
someUserErrorWrapped = SomeExceptionWithEq $ SomeException $ userError "Oh no"

-- * Helpers

runAndGetResults :: (HasCallStack) => TopSpec -> IO [Result]
runAndGetResults spec = do
  finalTree <- runSandwichTree defaultOptions spec
  fixedTree <- atomically $ mapM fixRunTree finalTree
  return $ fmap statusToResult $ concatMap getStatuses fixedTree

getStatuses :: (HasCallStack) => RunTreeWithStatus a l t -> [a]
getStatuses (RunTreeGroup {..}) = runTreeStatus : (concatMap getStatuses runTreeChildren)
getStatuses (RunTreeSingle {..}) = [runTreeStatus]

statusToResult :: (HasCallStack) => Status -> Result
statusToResult NotStarted = error "Expected status to be Done but was NotStarted"
statusToResult (Running {}) = error "Expected status to be Done but was Running"
statusToResult (Done _ _ result) = result

mustBe :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
mustBe x y
  | x == y = return ()
  | otherwise = error [i|Expected #{x} but got #{y}|]
