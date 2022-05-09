{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Maybe
import Data.Pool
import Data.String.Interpolate
import Data.Time.Clock
import System.FilePath
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Video
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands


-- * Introducing the pool

webDriverPool = Label :: Label "webDriverPool" (Pool WebDriver)
type HasWebDriverPool context = HasLabel context "webDriverPool" (Pool WebDriver)

introduceWebDriverPool :: forall m context. (
  Monad m, MonadIO m, MonadBaseControl IO m, HasBaseContext context, HasCommandLineOptions context ()
  ) => Int -> WdOptions -> SpecFree (LabelValue "webDriverPool" (Pool WebDriver) :> context) m () -> SpecFree context m ()
introduceWebDriverPool poolSize wdOptions' = introduce "Introduce webdriver pool" webDriverPool alloc cleanup
  where
    alloc = do
      clo <- getCommandLineOptions @()
      wdOptions <- addCommandLineOptionsToWdOptions clo wdOptions'
      runRoot <- fromMaybe "/tmp" <$> getRunRoot
      liftIO $ createPool (allocateWebDriver' runRoot wdOptions) cleanupWebDriver' 1 30 poolSize
    cleanup = liftIO . destroyAllResources

-- * Claiming a WebDriver

claimWebdriver spec = introduceWith' (
  defaultNodeOptions {nodeOptionsRecordTime=False, nodeOptionsCreateFolder=False}
  ) "Claim webdriver" webdriver wrappedAction spec
  where
    wrappedAction action = do
      pool <- getContext webDriverPool
      withResource pool $ \sess ->
        (void $ action sess) `finally` closeAllSessions sess

-- * Tests

tests :: TopSpecWithOptions
tests =
  introduceWebDriverPool 4 (defaultWdOptions "/tmp/tools") $
    parallel $
      replicateM_ 20 $
        claimWebdriver $ it "opens Google" $ withSession1 $ openPage "http://www.google.com"

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions tests
