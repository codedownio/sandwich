{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Sandwich.Formatters.Print.CallStacks (
  printCallStack
  , printSrcLoc
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import GHC.Stack
import System.IO (Handle)
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types


printCallStack :: (
  MonadReader (PrintFormatter, Int, Handle) m, MonadIO m
  ) => CallStack -> m ()
printCallStack cs = forM_ (getCallStack cs) printCallStackLine

printCallStackLine :: (
  MonadReader (PrintFormatter, Int, Handle) m, MonadIO m
  ) => (String, SrcLoc) -> m ()
printCallStackLine (f, srcLoc) = do
  pic logFunctionColor f

  p " called at "
  printSrcLoc srcLoc

printSrcLoc :: (
  MonadReader (PrintFormatter, Int, Handle) m, MonadIO m
  ) => SrcLoc -> m ()
printSrcLoc (SrcLoc {..}) = do
  pc logFilenameColor srcLocFile
  p ":"
  pc logLineColor (show srcLocStartLine)
  p ":"
  pc logChColor (show srcLocStartCol)
  p " in "
  pc logPackageColor srcLocPackage
  p ":"
  pc logModuleColor srcLocModule
  p "\n"

logFunctionColor = solarizedMagenta
logFilenameColor = solarizedViolet
logModuleColor = solarizedMagenta
logPackageColor = solarizedGreen
logLineColor = solarizedCyan
logChColor = solarizedOrange
