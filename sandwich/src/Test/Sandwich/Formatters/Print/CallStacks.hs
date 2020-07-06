{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.Print.CallStacks where

import Control.Monad
import GHC.Stack
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.Printing


printCallStack cs = forM_ (getCallStack cs) printCallStackLine

printCallStackLine (f, (SrcLoc {..})) = do
  pic logFunctionColor f

  p " called at "
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
