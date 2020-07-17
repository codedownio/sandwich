{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sandwich (
  runSandwich
  , runSandwichTree
  , startSandwichTree

  , it
  , describe
  , introduce
  , introduceWith
  , before
  , beforeEach
  , after
  , afterEach
  , around
  , aroundEach
  , parallel

  , TopSpec

  , defaultOptions
  , Options
  , optionsTestArtifactsDirectory
  , TestArtifactsDirectory(..)
  , optionsSavedLogLevel
  , optionsMemoryLogLevel
  , optionsFilterTree
  , optionsFormatters
  , TreeFilter(..)

  , BaseContext
  , HasBaseContext

  , Result(..)
  , FailureReason(..)
  , SomeExceptionWithEq(..)

  , SpecFree
  , SpecWith

  , ExampleT
  , ExampleM

  , Label(..)
  , LabelValue(..)
  , HasLabel
  , (:>)

  , SomeFormatter(..)
  , Formatter(..)

  , module Test.Sandwich.Contexts
  , module Test.Sandwich.Expectations
  , module Test.Sandwich.Logging

  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Logger
import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import System.Posix.Signals
import Test.Sandwich.Contexts
import Test.Sandwich.Expectations
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Interpreters.StartTree
import Test.Sandwich.Logging
import Test.Sandwich.Shutdown
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util


runSandwich :: Options -> TopSpec -> IO ()
runSandwich options spec = do
  baseContext <- baseContextFromOptions options
  rts <- startSandwichTree' baseContext options spec

  formatterAsyncs <- forM (optionsFormatters options) $ \(SomeFormatter f) -> async $ runFormatter f rts baseContext

  let shutdown = do
        putStrLn "Shutting down..."
        forM_ rts cancelNode
        -- cancel formatterAsync

  _ <- installHandler sigINT (Catch shutdown) Nothing

  putStrLn [i|Beginning wait for formatterAsync|]
  finalResults :: [Either E.SomeException ()] <- forM formatterAsyncs $ E.try . wait
  putStrLn [i|Final result: #{finalResults}|]

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
  _ <- mapM_ waitForTree rts
  return rts

baseContextFromOptions :: Options -> IO BaseContext
baseContextFromOptions options@(Options {..}) = do
  runRoot <- case optionsTestArtifactsDirectory of
    TestArtifactsNone -> return Nothing
    TestArtifactsFixedDirectory dir -> do
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

  errorCounter <- newMVar 0
  let errorSymlinksDir = (</> "errors") <$> runRoot
  whenJust errorSymlinksDir $ createDirectoryIfMissing True
  return $ BaseContext {
    baseContextPath = mempty
    , baseContextOptions = options
    , baseContextRunRoot = runRoot
    , baseContextErrorSymlinksDir = errorSymlinksDir
    , baseContextErrorCounter = errorCounter
    , baseContextOnlyRunIds = Nothing
    }

defaultOptions :: Options
defaultOptions = Options {
  optionsTestArtifactsDirectory = TestArtifactsNone
  , optionsSavedLogLevel = Just LevelDebug
  , optionsMemoryLogLevel = Just LevelDebug
  , optionsFilterTree = Nothing
  , optionsDryRun = False
  , optionsFormatters = [SomeFormatter defaultPrintFormatter]
  }
