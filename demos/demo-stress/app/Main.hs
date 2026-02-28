{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Text (Text)
import System.Random
import Test.Sandwich


-- | 400 test nodes running in parallel, each generating a few hundred log lines
-- over ~5 minutes total. Designed to stress-test the TUI's memory behavior
-- when all tests finish and statuses stabilize.
stressSpec :: TopSpec
stressSpec = parallel $ do
  forM_ [(1 :: Int)..400] $ \nodeId ->
    it [i|test #{nodeId}|] $ stressTest nodeId

stressTest :: Int -> ExampleM context ()
stressTest nodeId = do
  gen <- liftIO $ newStdGen
  -- Each test generates 100-300 log lines
  let logCount = 100 + (nodeId * 47 `mod` 200)
  -- Spread the work over the 5-minute window.
  -- Total run time per test: ~4-5 minutes with jitter.
  let baseSleepUs = (5 * 60 * 1000000) `div` logCount
  go gen logCount baseSleepUs (1 :: Int)
  where
    go _ 0 _ _ = return ()
    go gen remaining baseSleepUs lineNum = do
      let (jitter, gen') = uniformR (0, baseSleepUs `div` 2) gen
      let sleepUs = baseSleepUs + jitter - (baseSleepUs `div` 4)
      liftIO $ threadDelay sleepUs
      -- Generate log lines of varying verbosity
      let msg = makeLogMessage nodeId lineNum
      case lineNum `mod` 4 of
        0 -> debug msg
        1 -> info msg
        2 -> warn msg
        _ -> debug msg
      go gen' (remaining - 1) baseSleepUs (lineNum + 1)

makeLogMessage :: Int -> Int -> Text
makeLogMessage nodeId lineNum =
  let padding = replicate ((nodeId + lineNum) `mod` 80 + 20) '='
  in [i|[node=#{nodeId} line=#{lineNum}] Processing step #{lineNum}: #{padding} status=ok detail=#{detail lineNum}|]
  where
    detail n
      | n `mod` 10 == 0 = "checkpoint reached, flushing buffers and syncing state across all connected peers" :: Text
      | n `mod` 7 == 0 = "retrying operation after transient failure on upstream dependency"
      | n `mod` 5 == 0 = "cache miss, fetching from backing store"
      | n `mod` 3 == 0 = "validating intermediate result set"
      | otherwise = "nominal"

main :: IO ()
main = runSandwichWithCommandLineArgs options stressSpec
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
      }
