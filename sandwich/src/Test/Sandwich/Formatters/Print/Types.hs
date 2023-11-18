
module Test.Sandwich.Formatters.Print.Types where

import Control.Monad.Logger

data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  -- ^ Whether to use color in output. Defaults to 'True'.
  , printFormatterLogLevel :: Maybe LogLevel
  -- ^ Log level to show in output. Defaults to 'LevelWarn'.
  , printFormatterVisibilityThreshold :: Int
  -- ^ Visibility threshold. Nodes above this threshold will not be shown.
  , printFormatterIncludeCallStacks :: Bool
  -- ^ Whether to include callstacks with failures.
  , printFormatterIndentSize :: Int
  -- ^ The indentation unit in spaces. Defaults to 4.
  , printFormatterIncludeTimestamps :: IncludeTimestamps
  -- ^ Whether to include timestamps in output. Defaults to 'IncludeTimestampsNever'.
  } deriving (Show)

data IncludeTimestamps =
  IncludeTimestampsAlways
  | IncludeTimestampsNever
  | IncludeTimestampsFailuresOnly
  deriving (Show, Eq)

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterLogLevel = Just LevelWarn
  , printFormatterVisibilityThreshold = 50
  , printFormatterIncludeCallStacks = True
  , printFormatterIndentSize = 4
  , printFormatterIncludeTimestamps = IncludeTimestampsNever
  }
