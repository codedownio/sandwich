
module Test.Sandwich where

import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import System.Posix.Signals
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Types.Example
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.Spec

pending _ = return $ Pending Nothing Nothing

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
