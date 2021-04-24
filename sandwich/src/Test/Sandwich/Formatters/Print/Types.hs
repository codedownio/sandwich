
module Test.Sandwich.Formatters.Print.Types where

import Control.Monad.Logger

data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  , printFormatterLogLevel :: Maybe LogLevel
  , printFormatterVisibilityThreshold :: Int
  , printFormatterIncludeCallStacks :: Bool
  , printFormatterIndentSize :: Int
  } deriving (Show)

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterLogLevel = Just LevelWarn
  , printFormatterVisibilityThreshold = 50
  , printFormatterIncludeCallStacks = True
  , printFormatterIndentSize = 4
  }
