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
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Video
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Commands
import UnliftIO.Exception
import UnliftIO.Pool
import UnliftIO.Process


-- * Introducing the pool

webDriverPool = Label :: Label "webDriverPool" (Pool WebDriver)
type HasWebDriverPool context = HasLabel context "webDriverPool" (Pool WebDriver)

introduceWebDriverPool :: forall m context. (
  MonadUnliftIO m, MonadBaseControl IO m, MonadMask m
  , HasBaseContext context, HasSomeCommandLineOptions context, HasBrowserDependencies context, HasFile context "java", HasFile context "selenium.jar"
  ) => Int -> WdOptions -> SpecFree (LabelValue "webDriverPool" (Pool WebDriver) :> context) m () -> SpecFree context m ()
introduceWebDriverPool poolSize wdOptions' = introduceWith "Introduce webdriver pool" webDriverPool $ \action -> do
  wdOptions <- addCommandLineOptionsToWdOptions <$> getSomeCommandLineOptions <*> pure wdOptions'
  bracket (newPool =<< mkSafeDefaultPoolConfig (allocateWebDriver wdOptions) cleanupWebDriver 30.0 poolSize) destroyAllResources $ \pool ->
    void $ action pool

  where
    -- Based on https://hackage.haskell.org/package/unliftio-pool-0.4.2.0/docs/UnliftIO-Pool.html#v:mkDefaultPoolConfig
    -- Due to https://github.com/scrive/pool/issues/31
    mkSafeDefaultPoolConfig :: forall n a. MonadUnliftIO n => n a -> (a -> n ()) -> Double -> Int -> n (PoolConfig a)
    mkSafeDefaultPoolConfig create destroy keepAlive maxOpen = (setNumStripes (Just 1)) <$>
      mkDefaultPoolConfig create destroy keepAlive maxOpen

-- * Claiming a WebDriver

claimWebdriver spec = introduceWith' (
  defaultNodeOptions {nodeOptionsRecordTime=False, nodeOptionsCreateFolder=True}
  ) "Claim webdriver" webdriver wrappedAction spec
  where
    wrappedAction action = do
      pool <- getContext webDriverPool

      withResource pool $ \webdriver ->
        (void $ action webdriver) `finally` closeAllSessions webdriver

-- * Tests

tests :: TopSpecWithOptions
tests =
  introduceNixContext (nixpkgsReleaseDefault { nixpkgsDerivationAllowUnfree = True }) $
    introduceFileViaNixPackage' @"selenium.jar" "selenium-server-standalone" (findFirstFile (return . (".jar" `L.isSuffixOf`))) $
    introduceBinaryViaNixPackage @"java" "jre" $
    introduceBrowserDependenciesViaNix $
    introduceWebDriverPool 4 defaultWdOptions $
    parallel $
    replicateM_ 20 $
    claimWebdriver $ it "opens Google" $ withSession1 $ openPage "http://www.google.com"

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions tests
