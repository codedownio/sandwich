{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Test.Sandwich.WebDriver.Internal.Exceptions where

import Control.Concurrent
import Control.Monad
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import GHC.Stack
import System.FilePath
import System.IO
import Test.Sandwich.WebDriver.Internal.Types
import Test.WebDriver

#ifndef mingw32_HOST_OS
-- Note: one day, if directory-1.3.1.0 or later is ever on Stackage, we can use System.Directory.createDirectoryLink
import System.Posix.Files (createSymbolicLink)
#else
import Data.String
import Shelly hiding (sleep, (</>), FilePath, run)
#endif

saveSeleniumMessages :: (HasCallStack) => WdSession -> FilePath -> IO ()
saveSeleniumMessages session@(WdSession {..}) resultsDir = do
  sessionMap <- readMVar wdSessionMap
  forM_ (M.toList sessionMap) $ \(browser, sess) -> do
    hist <- runWD sess getSessionHistory
    withFile (resultsDir </> [i|#{browser}_selenium_messages.txt|]) WriteMode $ \h ->
      forM_ hist $ hPrint h
