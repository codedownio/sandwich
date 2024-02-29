{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Maybe
import Data.Pool
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Video
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands
import UnliftIO.Exception


-- * Introducing the pool

webDriverPool = Label :: Label "webDriverPool" (Pool WebDriver)
type HasWebDriverPool context = HasLabel context "webDriverPool" (Pool WebDriver)

introduceWebDriverPool :: forall m context. (
  MonadIO m, HasBaseContext context, HasCommandLineOptions context ()
  ) => Int -> WdOptions -> SpecFree (LabelValue "webDriverPool" (Pool WebDriver) :> context) m () -> SpecFree context m ()
introduceWebDriverPool poolSize wdOptions' = introduce "Introduce webdriver pool" webDriverPool alloc cleanup
  where
    alloc = do
      wdOptions <- addCommandLineOptionsToWdOptions <$> (getCommandLineOptions @()) <*> pure wdOptions'
      runRoot <- fromMaybe "/tmp" <$> getRunRoot
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

#if !MIN_VERSION_resource_pool(3,0,0)
      withRunInIO $ \runInIO ->
        withResource pool $ \sess ->
          runInIO $ (void $ action sess) `finally` closeAllSessions sess
#else
      withResource pool $ \sess ->
        (void $ action sess) `finally` closeAllSessions sess
#endif

-- * Tests

tests :: TopSpecWithOptions
tests =
  introduceWebDriverPool 4 (defaultWdOptions "/tmp/tools") $
    parallel $
      replicateM_ 20 $
        claimWebdriver $ it "opens Google" $ withSession1 $ openPage "http://www.google.com"

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions tests
