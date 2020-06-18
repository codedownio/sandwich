-- |

module Test.Sandwich.Types.Options where

import Control.Monad.Logger
import System.FilePath ()

-- | Control whether test artifacts are stored to a directory.
data TestArtifactsDirectory =
  TestArtifactsNone
  -- ^ Do not create a test artifacts directory.
  | TestArtifactsFixedDirectory {
      testRootFixed :: FilePath
      }
  -- ^ Use the test artifacts directory at the given path, creating it if necessary.
  | TestArtifactsGeneratedDirectory {
      runsRoot :: FilePath
      -- ^ The root folder under which test run directories will be created.
      , getTestRunDirectoryName :: IO FilePath
      -- ^ Action to generate the new directory name.
      }
  -- ^ Create a new test artifacts directory under '' test artifacts directory at the given path.

data Options = Options {
  optionsTestArtifactsDirectory :: TestArtifactsDirectory
  -- ^ Where to save test artifacts (logs, screenshots, failure reports, etc.)
  , optionsSaveTestLogs :: Maybe LogLevel
  -- ^ Minimum test log level to save (has no effect if 'optionsTestArtifactsDirectory' is 'TestArtifactsNone')
  }

defaultOptions :: Options
defaultOptions = Options {
  optionsTestArtifactsDirectory = TestArtifactsNone
  , optionsSaveTestLogs = Just LevelDebug
  }
