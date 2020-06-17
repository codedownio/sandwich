
module Test.Sandwich (
  runSandwich
  , runSandwichTree

  , it
  , describe
  , introduce
  , before
  , beforeEach
  , after
  , afterEach
  , around

  , defaultOptions

  , Result(..)
  , FailureReason(..)

  , module Test.Sandwich.Expectations

  ) where

import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import System.Posix.Signals
import Test.Sandwich.Expectations
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


runSandwich :: (Formatter f) => Options -> f -> TopSpec -> IO ()
runSandwich options f spec = do
  asyncUnit <- async $ return ()
  rts <- runReaderT (runTreeMain spec) $ RunTreeContext {
    runTreeContext = asyncUnit
    , runTreeOptions = options
    }

  formatterAsync <- async $ runFormatter f rts

  let shutdown = do
        putStrLn "TODO: shut down!"
        cancel formatterAsync

  _ <- installHandler sigINT (Catch shutdown) Nothing

  wait formatterAsync


runSandwichTree :: Options -> TopSpec -> IO [RunTree]
runSandwichTree options spec = do
  asyncUnit <- async $ return ()
  rts <- runReaderT (runTreeMain spec) $ RunTreeContext {
    runTreeContext = asyncUnit
    , runTreeOptions = options
    }

  mapM_ (wait . runTreeAsync) rts

  return rts
