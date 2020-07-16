-- |

module Test.Sandwich.Formatters.Print.Types where

import Control.Monad.Logger

data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  , printFormatterIncludeLogs :: Bool
  , printFormatterIncludeCallStacks :: Bool
  , printFormatterIndentSize :: Int
  , printFormatterLogLevel :: LogLevel
  }

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterIncludeLogs = True
  , printFormatterIncludeCallStacks = True
  , printFormatterIndentSize = 4
  , printFormatterLogLevel = LevelWarn
  }
