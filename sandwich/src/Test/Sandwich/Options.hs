{-# LANGUAGE CPP #-}

module Test.Sandwich.Options (
  Options
  , defaultOptions

  -- * Artifacts
  , optionsTestArtifactsDirectory
  , TestArtifactsDirectory(..)
  , defaultTestArtifactsDirectory

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

  -- * Long-running nodes
  , optionsWarnOnLongExecutionMs
  , optionsCancelOnLongExecutionMs

  -- * Folder path lengths
  , optionsMaxFolderPathLength
  , optionsFolderPathReserve
  , defaultMaxFolderPathLength

  -- * Dry run
  , optionsDryRun

  -- * Misc
  , optionsProjectRoot
  ) where

import Control.Monad.Logger
import Data.Function ((&))
import Data.Time.Clock
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Types.RunTree


-- | A reasonable default set of options.
defaultOptions :: Options
defaultOptions = Options {
  optionsTestArtifactsDirectory = TestArtifactsNone
  , optionsSavedLogLevel = Just LevelDebug
  , optionsMemoryLogLevel = Just LevelDebug
  , optionsLogFormatter = defaultLogEntryFormatter
  , optionsPruneTree = Nothing
  , optionsFilterTree = Nothing
  , optionsDryRun = False
  , optionsFormatters = [SomeFormatter defaultPrintFormatter]
  , optionsProjectRoot = Nothing
  , optionsTestTimerType = SpeedScopeTestTimerType { speedScopeTestTimerWriteRawTimings = False }
  , optionsWarnOnLongExecutionMs = Nothing
  , optionsCancelOnLongExecutionMs = Nothing
  , optionsLogBroadcast = Nothing
  , optionsEventBroadcast = Nothing
  , optionsLateLogFile = Nothing
  , optionsRunId = "run0"
  , optionsMaxFolderPathLength = defaultMaxFolderPathLength
  , optionsFolderPathReserve = 60
  }

-- | Windows' @MAX_PATH@ on Windows, unlimited elsewhere.
defaultMaxFolderPathLength :: Maybe Int
#ifdef mingw32_HOST_OS
defaultMaxFolderPathLength = Just 260
#else
defaultMaxFolderPathLength = Nothing
#endif

-- | Generate a test artifacts directory based on a timestamp.
--
-- Replace the colons with underscores, which makes this more safe for various
-- systems including Windows and GitHub Actions.
defaultTestArtifactsDirectory :: TestArtifactsDirectory
defaultTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" getFolderName
  where
    getFolderName = do
      ts <- show <$> getCurrentTime
      return $ ts
        & replace ':' '_'
        & replace ' ' '_'

    replace :: Eq a => a -> a -> [a] -> [a]
    replace a b = map $ \c -> if c == a then b else c
