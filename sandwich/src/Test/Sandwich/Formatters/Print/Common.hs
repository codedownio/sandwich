{-# LANGUAGE CPP #-}

module Test.Sandwich.Formatters.Print.Common where

import Control.Monad.Reader
import System.IO
import Test.Sandwich.Formatters.Print.CallStacks
import Test.Sandwich.Formatters.Print.Logs
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad
#endif


finishPrinting :: RunNodeCommon -> Result -> ReaderT (PrintFormatter, Int, Handle) IO ()
finishPrinting (RunNodeCommonWithStatus {..}) result = do
  includeCallStacks <- asks (printFormatterIncludeCallStacks . fst3)

  -- Print the callstack, if configured and present
  when includeCallStacks $ do
    case result of
      Failure (failureCallStack -> Just cs) -> do
        p "\n"
        withBumpIndent $ printCallStack cs
      _ -> return ()

  -- Print the logs, if configured
  printLogs runTreeLogs
