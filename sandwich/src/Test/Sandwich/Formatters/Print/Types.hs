-- |

module Test.Sandwich.Formatters.Print.Types where

import Control.Monad.Logger

data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  , printFormatterLogLevel :: Maybe LogLevel
  , printFormatterIncludeCallStacks :: Bool
  , printFormatterIndentSize :: Int
  }

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterLogLevel = Just LevelWarn
  , printFormatterIncludeCallStacks = True
  , printFormatterIndentSize = 4
  }
