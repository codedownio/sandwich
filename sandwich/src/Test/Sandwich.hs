{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sandwich (

  -- * Running tests
  runSandwich
  , runSandwich'
  , runSandwichWithCommandLineArgs
  , runSandwichTree
  , startSandwichTree

  -- * Basic nodes
  , it
  , describe
  , parallel

  -- * Context manager nodes
  , introduce
  , introduceWith
  , before
  , beforeEach
  , after
  , afterEach
  , around
  , aroundEach

  -- * Timing
  , timeActionByProfile
  , timeAction
  , withTimingProfile

  -- * The example monad
  , ExampleT
  , ExampleM

  -- * Spec types
  , Spec
  , SpecFree
  , TopSpec

  , BaseContext
  , HasBaseContext

  , Label(..)
  , LabelValue(..)
  , HasLabel
  , (:>)

  , isEmptySpec

  , SomeExceptionWithCallStack(..)

  , ExitReason(..)

  , module Test.Sandwich.Contexts
  , module Test.Sandwich.Expectations
  , module Test.Sandwich.Logging
  , module Test.Sandwich.Options
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Either
import Data.IORef
import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import System.Posix.Signals
import Test.Sandwich.ArgParsing
import Test.Sandwich.Contexts
import Test.Sandwich.Expectations
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Interpreters.StartTree
import Test.Sandwich.Logging
import Test.Sandwich.Options
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.General
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import Test.Sandwich.Util


-- | Run the spec
runSandwich :: Options -> TopSpec -> IO ()
runSandwich options spec = void $ runSandwich' options spec

-- | Run the spec, configuring the options from the command line
runSandwichWithCommandLineArgs :: Options -> TopSpec -> IO ()
runSandwichWithCommandLineArgs baseOptions spec = do
  (options, repeatCount) <- liftIO $ addOptionsFromArgs baseOptions
  void $ runSandwich' options spec

-- | Run the spec and return the number of failures
runSandwich' :: Options -> TopSpec -> IO (ExitReason, Int)
runSandwich' options spec' = do
  baseContext <- baseContextFromOptions options

  -- Wrap the spec in a finalizer for the test timer, when one is present
  let spec = case baseContextTestTimer baseContext of
        NullTestTimer -> spec'
        _ -> after' (defaultNodeOptions { nodeOptionsRecordTime = False }) "Finalize test timer" (asks getTestTimer >>= liftIO . finalizeSpeedScopeTestTimer) spec'

  rts <- startSandwichTree' baseContext options spec

  formatterAsyncs <- forM (optionsFormatters options) $ \(SomeFormatter f) -> async $ do
    let loggingFn = case baseContextRunRoot baseContext of
          Nothing -> flip runLoggingT (\_ _ _ _ -> return ())
          Just rootPath -> runFileLoggingT (rootPath </> (formatterName f) <.> "log")

    loggingFn $
      runFormatter f rts baseContext

  exitReasonRef <- newIORef NormalExit

  let shutdown = do
        putStrLn "Shutting down..."
        writeIORef exitReasonRef InterruptExit
        forM_ rts cancelNode

  _ <- installHandler sigINT (Catch shutdown) Nothing

  -- Wait for all formatters to finish
  putStrLn [i|Beginning wait for formatterAsync|]
  finalResults :: [Either E.SomeException ()] <- forM formatterAsyncs $ E.try . wait
  let failures = lefts finalResults
  unless (null failures) $
    putStrLn [i|Some formatters failed: '#{failures}'|]

  -- Run finalizeFormatter method on formatters
  forM_ (optionsFormatters options) $ \(SomeFormatter f) -> do
    let loggingFn = case baseContextRunRoot baseContext of
          Nothing -> flip runLoggingT (\_ _ _ _ -> return ())
          Just rootPath -> runFileLoggingT (rootPath </> (formatterName f) <.> "log")

    loggingFn $ finalizeFormatter f rts baseContext

  fixedTree <- atomically $ mapM fixRunTree rts
  let failed = countWhere isFailedItBlock fixedTree
  exitReason <- readIORef exitReasonRef
  return (exitReason, failed)

startSandwichTree :: Options -> TopSpec -> IO [RunNode BaseContext]
startSandwichTree options spec = do
  baseContext <- baseContextFromOptions options
  startSandwichTree' baseContext options spec

startSandwichTree' :: BaseContext -> Options -> TopSpec -> IO [RunNode BaseContext]
startSandwichTree' baseContext (Options {..}) spec' = do
  let spec = case optionsFilterTree of
        Nothing -> spec'
        Just (TreeFilter match) -> filterTree match spec'

  runTree <- atomically $ specToRunTreeVariable baseContext spec

  unless optionsDryRun $ do
    void $ async $ void $ runNodesSequentially runTree baseContext

  return runTree

runSandwichTree :: Options -> TopSpec -> IO [RunNode BaseContext]
runSandwichTree options spec = do
  rts <- startSandwichTree options spec
  mapM_ waitForTree rts
  return rts

baseContextFromOptions :: Options -> IO BaseContext
baseContextFromOptions options@(Options {..}) = do
  runRoot <- case optionsTestArtifactsDirectory of
    TestArtifactsNone -> return Nothing
    TestArtifactsFixedDirectory dir' -> do
      dir <- case isAbsolute dir' of
        True -> return dir'
        False -> do
          here <- getCurrentDirectory
          return $ here </> dir'

      createDirectoryIfMissing True dir
      return $ Just dir
    TestArtifactsGeneratedDirectory base' f -> do
      base <- case isAbsolute base' of
        True -> return base'
        False -> do
          here <- getCurrentDirectory
          return $ here </> base'

      name <- f
      let dir = base </> name
      createDirectoryIfMissing True dir
      return $ Just dir

  testTimer <- case (optionsTestTimerType, runRoot) of
    (SpeedScopeTestTimerType, Just rr) -> liftIO $ newSpeedScopeTestTimer rr
    _ -> return NullTestTimer

  let errorSymlinksDir = (</> "errors") <$> runRoot
  whenJust errorSymlinksDir $ createDirectoryIfMissing True
  return $ BaseContext {
    baseContextPath = mempty
    , baseContextOptions = options
    , baseContextRunRoot = runRoot
    , baseContextErrorSymlinksDir = errorSymlinksDir
    , baseContextOnlyRunIds = Nothing
    , baseContextTestTimerProfile = defaultProfileName
    , baseContextTestTimer = testTimer
    }
