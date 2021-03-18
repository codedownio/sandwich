{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Test.Sandwich.Internal.Running where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Data.Function
import Data.Maybe
import Data.String.Interpolate
import System.Directory
import System.Exit
import System.FilePath
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Interpreters.StartTree
import Test.Sandwich.Options
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.General
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import Test.Sandwich.Util


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

-- | For 0 repeats, repeat until a failure
runWithRepeat :: Int -> IO (ExitReason, Int) -> IO ()
runWithRepeat 0 action = do
  (_, numFailures) <- action
  if | numFailures == 0 -> runWithRepeat 0 action
     | otherwise -> exitFailure
-- | For 1 repeat, run once and return
runWithRepeat n action = do
  (successes, total) <- (flip execStateT (0 :: Int, 0 :: Int)) $ flip fix (n - 1) $ \loop n -> do
    (exitReason, numFailures) <- liftIO action

    modify $ \(successes, total) -> (successes + (if numFailures == 0 then 1 else 0), total + 1)

    if | exitReason == InterruptExit -> return ()
       | n > 0 -> loop (n - 1)
       | otherwise -> return ()

  putStrLn [i|#{successes} runs succeeded out of #{total} repeats|]

  when (successes /= total) $ exitFailure    
    
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
    (SpeedScopeTestTimerType {..}, Just rr) -> liftIO $ newSpeedScopeTestTimer rr speedScopeTestTimerWriteRawTimings
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
    

-- | Gather all node options from a spec
gatherNodeOptions :: Free (SpecCommand context m) r -> [NodeOptions]
gatherNodeOptions (Free x@(It'' {})) = (nodeOptions x) : gatherNodeOptions (next x)
gatherNodeOptions (Free (IntroduceWith'' {..})) = nodeOptions : (gatherNodeOptions next <> gatherNodeOptions subspecAugmented)
gatherNodeOptions (Free (Introduce'' {..})) = nodeOptions : (gatherNodeOptions next <> gatherNodeOptions subspecAugmented)
gatherNodeOptions (Free x) = (nodeOptions x) : (gatherNodeOptions (next x) <> gatherNodeOptions (subspec x))
gatherNodeOptions (Pure _) = []

gatherMainFunctions :: Free (SpecCommand context m) r -> [NodeMainFunction]
gatherMainFunctions tests = gatherNodeOptions tests
                            & fmap nodeOptionsMainFunction
                            & catMaybes

