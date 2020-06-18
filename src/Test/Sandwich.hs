{-# LANGUAGE RecordWildCards #-}

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
import System.Directory
import System.FilePath
import System.Posix.Signals
import Test.Sandwich.Expectations
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


runSandwich :: (Formatter f) => Options -> f -> TopSpec -> IO ()
runSandwich options f spec = do
  rts <- startSandwichTree options spec

  formatterAsync <- async $ runFormatter f rts

  let shutdown = do
        putStrLn "TODO: shut down!"
        cancel formatterAsync

  _ <- installHandler sigINT (Catch shutdown) Nothing

  wait formatterAsync

startSandwichTree :: Options -> TopSpec -> IO [RunTree]
startSandwichTree options@(Options {..}) spec = do
  runRoot <- case optionsTestArtifactsDirectory of
    TestArtifactsNone -> return Nothing
    TestArtifactsFixedDirectory dir -> do
      createDirectoryIfMissing True dir
      return $ Just dir
    TestArtifactsGeneratedDirectory base f -> do
      name <- f
      let dir = base </> name
      createDirectoryIfMissing True dir
      return $ Just dir

  asyncBaseContext <- async $ return $ BaseContext {
    baseContextPath = mempty
    , baseContextOptions = options
    , baseContextRunRoot = runRoot
    }

  rts <- runReaderT (runTreeMain spec) $ RunTreeContext {
    runTreeContext = asyncBaseContext
    , runTreeOptions = options
    }

  return rts

runSandwichTree :: Options -> TopSpec -> IO [RunTree]
runSandwichTree options spec = do
  rts <- startSandwichTree options spec
  mapM_ (wait . runTreeAsync) rts
  return rts
