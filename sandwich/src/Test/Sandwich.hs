{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Test.Sandwich (

  -- * Running tests
  runSandwich
  , runSandwich'
  , runSandwichWithCommandLineArgs
  , runSandwichWithCommandLineArgs'

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
  , withTimingProfile'

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
  , module Test.Sandwich.TH
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Either
import Data.Function
import Data.IORef
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import Options.Applicative
import qualified Options.Applicative as OA
import System.Environment
import System.FilePath
import System.Posix.Signals
import Test.Sandwich.ArgParsing
import Test.Sandwich.Contexts
import Test.Sandwich.Expectations
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Internal.Running
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Logging
import Test.Sandwich.Options
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.TH
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.General
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer


-- | Run the spec
runSandwich :: Options -> TopSpec -> IO ()
runSandwich options spec = void $ runSandwich' options spec

-- | Run the spec, configuring the options from the command line
runSandwichWithCommandLineArgs :: Options -> TopSpec -> IO ()
runSandwichWithCommandLineArgs baseOptions spec = runSandwichWithCommandLineArgs' baseOptions (pure ()) (const spec)

-- | Run the spec, configuring the options from the command line and adding user-configured command line options
runSandwichWithCommandLineArgs' :: Options -> Parser a -> (a -> TopSpec) -> IO ()
runSandwichWithCommandLineArgs' baseOptions userOptionsParser spec = do
  clo <- OA.execParser (commandLineOptionsWithInfo userOptionsParser)
  (options, repeatCount) <- liftIO $ addOptionsFromArgs baseOptions clo

  if | optPrintSlackFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser slackOptionsWithInfo
     | optPrintWebDriverFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser webDriverOptionsWithInfo
     | optListAvailableTests clo == Just True -> do
         let mainFunctions = gatherMainFunctions (spec undefined)
                             & L.sortOn nodeModuleInfoModuleName
         let modulesAndShorthands = gatherShorthands mainFunctions
         forM_ modulesAndShorthands $ \(NodeModuleInfo {..}, shorthand) -> do
           let hasMain = if isJust nodeModuleInfoFn then " (*)" else ("" :: String)
           putStrLn [i|#{nodeModuleInfoModuleName}#{hasMain}: #{shorthand}|]
     | otherwise -> do
         runWithRepeat repeatCount $
           runSandwich' options (spec (optUserOptions clo))

-- | Run the spec and return the number of failures
runSandwich' :: Options -> TopSpec -> IO (ExitReason, Int)
runSandwich' options spec' = do
  baseContext <- baseContextFromOptions options

  -- Wrap the spec in a finalizer for the test timer, when one is present
  let spec = case baseContextTestTimer baseContext of
        NullTestTimer -> spec'
        _ -> after' (defaultNodeOptions { nodeOptionsRecordTime = False
                                        , nodeOptionsCreateFolder = False }) "Finalize test timer" (asks getTestTimer >>= liftIO . finalizeSpeedScopeTestTimer) spec'

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
