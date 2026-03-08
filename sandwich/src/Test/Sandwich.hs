{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich (
  -- | Sandwich is a test framework for Haskell. See the <https://codedownio.github.io/sandwich/docs/ documentation> for details and usage examples.

  -- * Running tests with command line args
  --
  -- | These functions will read command line arguments when setting up your tests.
  -- These flags allow you filter the test tree, configure formatters, and pass your own custom options.
  --
  -- @
  -- # Run using the terminal UI formatter, webdriver headless mode, filtering to nodes matching \"Login\"
  -- stack run my-tests -- --tui --headless -f Login
  -- @
  --
  runSandwichWithCommandLineArgs
  , runSandwichWithCommandLineArgs'
  , parseCommandLineArgs

  -- * Running tests
  , runSandwich
  , runSandwich'

  -- * Basic nodes
  --
  -- | The basic building blocks of tests.
  , it
  , describe
  , parallel

  -- * Context manager nodes
  --
  -- | For introducing new contexts into tests and doing setup/teardown.
  , introduce
  , introduceWith
  , before
  , beforeEach
  , after
  , afterEach
  , around
  , aroundEach

  -- * Parallel nodes
  , module Test.Sandwich.ParallelN

  -- * Timing
  --
  -- | For timing actions within your tests. Test tree nodes are timed by default.
  , timeActionByProfile
  , timeAction
  , withTimingProfile
  , withTimingProfile'

  -- * Managed async
  --
  -- | If you want to run asyncs within your tests, we can help keep track of
  -- them and make sure they get cleaned up.
  , managedAsync
  , managedAsyncWithUnmask
  , managedWithAsync
  , managedWithAsync_

  -- * Exports
  , module Test.Sandwich.Contexts
  , module Test.Sandwich.Expectations
  , module Test.Sandwich.Logging
  , module Test.Sandwich.Misc
  , module Test.Sandwich.Nodes
  , module Test.Sandwich.Options
  , module Test.Sandwich.TH
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Function
import Data.IORef
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Debug.Trace (traceMarkerIO)
import GHC.IO.Encoding
import Options.Applicative
import qualified Options.Applicative as OA
import System.Environment
import System.Exit
import System.FilePath
import System.IO (hSetBuffering, BufferMode(..), IOMode(..), openFile, hClose)
import Test.Sandwich.ArgParsing
import Test.Sandwich.Contexts
import Test.Sandwich.Expectations
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Golden.Update
import Test.Sandwich.Instrumentation
import Test.Sandwich.Internal.Running
import Test.Sandwich.Interpreters.FilterTreeModule
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Logging
import qualified Test.Sandwich.ManagedAsync as MA
import Test.Sandwich.Misc
import Test.Sandwich.Nodes
import Test.Sandwich.Options
import Test.Sandwich.ParallelN
import Test.Sandwich.RunTree
import Test.Sandwich.Shutdown
import Test.Sandwich.Signals
import Test.Sandwich.TH
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import UnliftIO.Exception
import UnliftIO.Timeout (timeout)

#ifdef mingw32_HOST_OS
import System.Win32.Console (setConsoleOutputCP)
#endif


-- | Run the spec with the given 'Options'.
runSandwich :: Options -> CoreSpec -> IO ()
runSandwich options spec = do
  (_exitReason, _itNodeFailures, totalFailures) <- runSandwich' Nothing options spec
  when (0 < totalFailures) exitFailure

-- | Run the spec, configuring the options from the command line.
runSandwichWithCommandLineArgs :: Options -> TopSpecWithOptions -> IO ()
runSandwichWithCommandLineArgs baseOptions = runSandwichWithCommandLineArgs' baseOptions (pure ())

-- | Run the spec, configuring the options from the command line and adding user-configured command line options.
-- The options will become available as a test context, which you can access by calling 'getCommandLineOptions'.
runSandwichWithCommandLineArgs' :: forall a. (Typeable a) => Options -> Parser a -> TopSpecWithOptions' a -> IO ()
runSandwichWithCommandLineArgs' baseOptions userOptionsParser spec = do
  (clo, individualTestParser, modulesAndShorthands) <- parseCommandLineArgs' userOptionsParser spec
  (options, repeatCount) <- liftIO $ addOptionsFromArgs baseOptions clo

  if | optPrintGoldenFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser goldenOptionsWithInfo
     | optPrintHedgehogFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser hedgehogOptionsWithInfo
     | optPrintQuickCheckFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser quickCheckOptionsWithInfo
     | optPrintSlackFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser slackOptionsWithInfo
     | optPrintWebDriverFlags clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser webDriverOptionsWithInfo
     | optListAvailableTests clo == Just True -> do
         void $ withArgs ["--help"] $
           OA.execParser $ OA.info (individualTestParser mempty <**> helper) $
             fullDesc <> header "Pass one of these flags to run an individual test module."
                      <> progDesc "If a module has a \"*\" next to its name, then we detected that it has its own main function. If you pass the option name suffixed by -main then we'll just directly invoke the main function."
     | optListAvailableTestsJson clo == Just True -> do
         BL.putStr $ A.encode [M.fromList [("module" :: T.Text, nodeModuleInfoModuleName), ("flag", "--" <> T.unpack shorthand)]
                              | (NodeModuleInfo {..}, shorthand) <- modulesAndShorthands]
     | optUpdateGolden (optGoldenOptions clo) == Just True -> do
         updateGolden (optGoldenDir (optGoldenOptions clo))
     | otherwise -> do
         -- Awkward, but we need a specific context type to call countItNodes
         let totalTests = countItNodes (spec :: SpecFree (LabelValue "someCommandLineOptions" SomeCommandLineOptions :> LabelValue "commandLineOptions" (CommandLineOptions a) :> BaseContext) IO ())

         let cliNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = systemVisibilityThreshold
                                                 , nodeOptionsCreateFolder = False }

         let mkRunId idx = if repeatCount == 1 then "run" else [i|repeat-#{idx}|]
         runWithRepeat repeatCount totalTests $ \repeatIdx -> do
           let opts = options { optionsRunId = mkRunId repeatIdx }
           case optIndividualTestModule clo of
             Nothing -> runSandwich' (Just $ clo { optUserOptions = () }) opts $
               introduce' cliNodeOptions "some command line options" someCommandLineOptions (pure (SomeCommandLineOptions clo)) (const $ return ())
                 $ introduce' cliNodeOptions "command line options" commandLineOptions (pure clo) (const $ return ()) spec
             Just (IndividualTestModuleName x) -> runSandwich' (Just $ clo { optUserOptions = () }) opts $ filterTreeToModule x $
               introduce' cliNodeOptions "some command line options" someCommandLineOptions (pure (SomeCommandLineOptions clo)) (const $ return ())
                 $ introduce' cliNodeOptions "command line options" commandLineOptions (pure clo) (const $ return ()) spec
             Just (IndividualTestMainFn x) -> do
               let individualTestFlagStrings = [[ Just ("--" <> shorthand), ("--" <> shorthand <> "-main") <$ nodeModuleInfoFn ]
                                               | (NodeModuleInfo {..}, shorthand) <- modulesAndShorthands]
                                             & mconcat
                                             & catMaybes
               baseArgs <- getArgs
               withArgs (baseArgs L.\\ (fmap T.unpack individualTestFlagStrings)) $
                 tryAny x >>= \case
                   Left _ -> return (NormalExit, 1, 1)
                   Right _ -> return (NormalExit, 0, 0)

-- | Run the spec with optional custom 'CommandLineOptions'. When finished, return the exit reason,
-- the number of "it" nodes which failed, and the total failed nodes (which includes "it" nodes, and
-- may also include other nodes like "introduce" nodes).
runSandwich' :: Maybe (CommandLineOptions ()) -> Options -> CoreSpec -> IO (ExitReason, Int, Int)
runSandwich' maybeCommandLineOptions options spec' = do
  let runId = optionsRunId options
  baseContext <- baseContextFromOptions options

  -- Open late-log file if --log-logs is active
  maybeLateLogHandle <- case (maybeCommandLineOptions, baseContextRunRoot baseContext) of
    (Just clo, Just runRoot) | optLogLogs clo -> do
      h <- openFile (runRoot </> "late-logs.log") AppendMode
      hSetBuffering h LineBuffering
      return (Just h)
    _ -> return Nothing

  let options' = options { optionsLateLogFile = maybeLateLogHandle }

  let milestone msg = do
        traceMarkerIO [i|sandwich: #{msg}|]
        now <- getCurrentTime
        case optionsEventBroadcast options' of
          Just chan -> atomically $ writeTChan chan (NodeEvent now 0 "sandwich" (EventMilestone msg))
          Nothing -> return ()

  -- To prevent weird errors saving files like "commitAndReleaseBuffer: invalid argument (invalid character)",
  -- especially on Windows
  -- See https://gitlab.haskell.org/ghc/ghc/issues/8118
  setLocaleEncoding utf8
#ifdef mingw32_HOST_OS
  -- Fix Windows console output. Makes sandwich-hedgehog unicode print properly
  setConsoleOutputCP 65001
#endif

  -- Wrap the spec in a finalizer for the test timer, when one is present
  let spec = case baseContextTestTimer baseContext of
        NullTestTimer -> spec'
        _ -> after' (defaultNodeOptions {
                        nodeOptionsRecordTime = False
                        , nodeOptionsVisibilityThreshold = systemVisibilityThreshold
                        , nodeOptionsCreateFolder = False
                        }) "Finalize test timer" (asks getTestTimer >>= liftIO . finalizeSpeedScopeTestTimer) spec'

  milestone "startSandwichTree'"
  rts <- startSandwichTree' baseContext options' spec

  milestone "spawning formatters"
  formatterAsyncs <- forM (optionsFormatters options') $ \(SomeFormatter f) ->
    MA.managedAsync runId (T.pack [i|formatter:#{formatterName f}|]) $ do
      let loggingFn = case baseContextRunRoot baseContext of
            Nothing -> flip runLoggingT (\_ _ _ _ -> return ())
            Just rootPath -> runFileLoggingT (rootPath </> (formatterName f) <.> "log")

      loggingFn $
        runFormatter f rts maybeCommandLineOptions baseContext

  -- Spawn file writer asyncs for --log-logs, --log-events, --log-rts-stats, --log-asyncs
  milestone "spawning file stream asyncs"
  (fileStreamAsyncs, maybeEventStreamAsync, maybeManagedAsyncStreamAsync) <- case (maybeCommandLineOptions, baseContextRunRoot baseContext) of
    (Just clo, Just runRoot) -> do
      others <- fmap catMaybes $ sequence
        [ if optLogLogs clo
          then case optionsLogBroadcast options' of
            Just chan -> Just <$> MA.managedAsync runId "stream-logs" (streamLogsToFile (runRoot </> "all-logs.log") chan)
            Nothing -> return Nothing
          else return Nothing
        , if optLogRtsStats clo
          then Just <$> MA.managedAsync runId "stream-rts-stats" (streamRtsStatsToFile (runRoot </> "rts-stats.log"))
          else return Nothing
        ]
      -- Spawn stream-events separately so we can drain it gracefully via EventEndOfStream
      maybeEvtAsync <- if optLogEvents clo
        then do
          writeTreeFile (runRoot </> "events-tree.txt") rts
          case optionsEventBroadcast options' of
            Just chan -> Just <$> MA.managedAsync runId "stream-events" (streamEventsToFile (runRoot </> "events.log") chan)
            Nothing -> return Nothing
        else return Nothing
      -- Spawn the managed-async event stream separately so we can cancel it last
      maybeManagedAsync <- if optLogAsyncs clo
        then Just <$> MA.managedAsync runId "stream-managed-asyncs" (streamManagedAsyncEventsToFile (runRoot </> "asyncs.log") MA.asyncEventBroadcast)
        else return Nothing
      return (others, maybeEvtAsync, maybeManagedAsync)
    _ -> return ([], Nothing, Nothing)

  exitReasonRef <- newIORef NormalExit

  let shutdown sig = do
        let signalName :: T.Text =
              if | sig == sigINT -> "sigINT"
                 | sig == sigTERM -> "sigTERM"
                 | otherwise -> [i|signal #{sig}|]
        putStrLn [i|Shutting down due to #{signalName}...|]
        writeIORef exitReasonRef SignalExit
        forM_ rts cancelNode

  _ <- installHandler sigINT shutdown
  _ <- installHandler sigTERM shutdown

  -- Wait for the tree to finish
  milestone "waiting for tree"
  mapM_ waitForTree rts
  milestone "tree finished"

  -- Wait for all formatters to finish
  milestone "waiting for formatters"
  finalResults :: [Either E.SomeException ()] <- forM formatterAsyncs $ E.try . wait
  let failures = lefts finalResults
  unless (null failures) $
    putStrLn [i|Some formatters failed: '#{failures}'|]
  milestone "formatters finished"

  -- Run finalizeFormatter method on formatters
  milestone [i|finalizing formatters: #{optionsFormatters options'}|]
  forM_ (optionsFormatters options') $ \(SomeFormatter f) -> do
    milestone [i|finalizing formatter: #{f}|]

    let loggingFn = case baseContextRunRoot baseContext of
          Nothing -> flip runLoggingT (\_ _ _ _ -> return ())
          Just rootPath -> runFileLoggingT (rootPath </> (formatterName f) <.> "log")

    loggingFn $ finalizeFormatter f rts baseContext

  milestone "fixing tree"
  fixedTree <- atomically $ mapM fixRunTree rts

  -- Close late-log file handle
  mapM_ hClose maybeLateLogHandle

  -- Emit the "done" milestone before draining the event stream
  milestone "done"

  -- Signal the event stream to stop and wait for it to drain (with 60s timeout)
  case optionsEventBroadcast options' of
    Just chan -> do
      now <- getCurrentTime
      atomically $ writeTChan chan (NodeEvent now 0 "sandwich" EventEndOfStream)
    Nothing -> return ()
  forM_ maybeEventStreamAsync $ \asy ->
    timeout 60_000_000 (wait asy) >>= \case
      Nothing -> do
        putStrLn "WARNING: stream-events async didn't exit within 60s, cancelling"
        cancel asy
      Just _ -> return ()

  -- Cancel remaining file stream asyncs (but not the managed-async stream yet)
  mapM_ cancel fileStreamAsyncs

  -- Cancel the managed-async stream last, so its finally block captures the final state
  mapM_ cancel maybeManagedAsyncStreamAsync

  -- Check for stale managed asyncs from this run
  milestone "checking for stale asyncs"
  asyncs <- MA.getManagedAsyncInfos
  unless (M.null asyncs) $ do
    putStrLn [i|WARNING: #{M.size asyncs} managed asyncs still running after tree finished:|]
    forM_ (M.toList asyncs) $ \(tid, x) ->
      putStrLn [i|  #{tid}: #{MA.asyncInfoName x}|]

  exitReason <- readIORef exitReasonRef
  let failedItBlocks = countWhere isFailedItBlock fixedTree
  let failedBlocks = countWhere isFailedBlock fixedTree
  return (exitReason, failedItBlocks, failedBlocks)


-- | Count the it nodes
countItNodes :: Free (SpecCommand context m) r -> Int
countItNodes (Free x@(It'' {})) = 1 + countItNodes (next x)
countItNodes (Free (IntroduceWith'' {..})) = countItNodes next + countItNodes subspecAugmented
countItNodes (Free (Introduce'' {..})) = countItNodes next + countItNodes subspecAugmented
countItNodes (Free x) = countItNodes (next x) + countItNodes (subspec x)
countItNodes (Pure _) = 0

-- * Managed async (context-aware aliases)

-- | Launch a managed async thread, tracking it with the run ID from 'BaseContext'.
managedAsync :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> m a -> m (Async a)
managedAsync = MA.managedAsyncContext

-- | Like 'managedAsync', but the action receives an unmask function.
managedAsyncWithUnmask :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> ((forall b. m b -> m b) -> m a) -> m (Async a)
managedAsyncWithUnmask = MA.managedAsyncWithUnmaskContext

-- | Run a managed async thread scoped to a callback.
managedWithAsync :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> m a -> (Async a -> m b) -> m b
managedWithAsync = MA.managedWithAsyncContext

-- | Like 'managedWithAsync', but ignores the 'Async' handle.
managedWithAsync_ :: (MonadUnliftIO m, HasBaseContextMonad context m) => T.Text -> m a -> m b -> m b
managedWithAsync_ = MA.managedWithAsyncContext_
