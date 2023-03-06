
module Test.Sandwich.Options (
  Options
  , defaultOptions

  -- * Artifacts
  , optionsTestArtifactsDirectory
  , TestArtifactsDirectory(..)

  -- * Logging
  , optionsSavedLogLevel
  , optionsMemoryLogLevel
  , optionsLogFormatter
  , LogEntryFormatter

  -- * Formatting
  , optionsFormatters
  , SomeFormatter(..)
  , Formatter(..)

  -- * Filtering
  , optionsFilterTree
  , optionsPruneTree
  , TreeFilter(..)

  -- * Timing
  , optionsTestTimerType

  -- * Dry run
  , optionsDryRun

  -- * Misc
  , optionsProjectRoot
  ) where

import Control.Monad.Logger
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Types.RunTree

-- | A reasonable default set of options.
defaultOptions :: Options
defaultOptions = Options {
  optionsTestArtifactsDirectory = TestArtifactsNone
  , optionsSavedLogLevel = Just LevelDebug
  , optionsMemoryLogLevel = Just LevelDebug
  , optionsLogFormatter = defaultLogEntryFormatter
  , optionsFilterTree = Nothing
  , optionsPruneTree = Nothing
  , optionsDryRun = False
  , optionsFormatters = [SomeFormatter defaultPrintFormatter]
  , optionsProjectRoot = Nothing
  , optionsTestTimerType = SpeedScopeTestTimerType { speedScopeTestTimerWriteRawTimings = False }
  }
