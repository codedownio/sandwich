{-# LANGUAGE DataKinds #-}

module UnicodeLabels (
  tests
  , main
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import qualified Data.List as L
import GHC.IO.Encoding
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO
import System.Timeout (timeout)
import Test.Sandwich
import Test.Sandwich.Formatters.FailureReport
import TestUtil
import UnliftIO.Exception


tests :: MonadIO m => FilePath -> WriterT [SomeException] m ()
tests testRunsDir = do
  run (unicodeLabelsWorkUnderAsciiLocale testRunsDir)

main :: IO ()
-- Not point-free (mainWith . tests), since GHC 9.0 rejects that for mainWith's constrained argument
main = getTestRunsDir >>= \testRunsDir -> mainWith (tests testRunsDir)

-- * Tests

-- | Non-ASCII labels should become folders and print, even under an ASCII locale like @LANG=C@.
unicodeLabelsWorkUnderAsciiLocale :: (HasCallStack) => FilePath -> IO ()
unicodeLabelsWorkUnderAsciiLocale testRunsDir = withTempArtifactsDir testRunsDir $ \dir -> withAsciiEncodings $ do
  let options = defaultOptions {
        optionsTestArtifactsDirectory = TestArtifactsFixedDirectory dir
        , optionsFormatters = [SomeFormatter defaultFailureReportFormatter]
        }
  result <- timeout 30_000_000 $ runSandwich' Nothing options $
    describe "outer…" $ do
      it "passes…" $ return ()
      it "is pending…" pending

  result `mustBe` Just (NormalExit, 0, 0)

  folders <- listDirectoriesRecursive dir
  unless (L.any (("passes…" `L.isSuffixOf`) . takeFileName) folders) $
    expectationFailure ("Expected a folder for 'passes…', got: " <> show folders)

-- | Run with ASCII file system and stdout encodings.
withAsciiEncodings :: IO a -> IO a
withAsciiEncodings action = bracket useAsciiEncodings restoreEncodings (const action)
  where
    useAsciiEncodings = do
      oldEncodings <- (,) <$> getFileSystemEncoding <*> hGetEncoding stdout
      asciiRoundtrip <- mkTextEncoding "ASCII//ROUNDTRIP"
      ascii <- mkTextEncoding "ASCII"
      setFileSystemEncoding asciiRoundtrip
      hSetEncoding stdout ascii
      return oldEncodings

    restoreEncodings (oldFileSystemEncoding, oldStdoutEncoding) = do
      setFileSystemEncoding oldFileSystemEncoding
      mapM_ (hSetEncoding stdout) oldStdoutEncoding

listDirectoriesRecursive :: FilePath -> IO [FilePath]
listDirectoriesRecursive dir = do
  children <- fmap (dir </>) <$> listDirectory dir
  childDirs <- filterM doesDirectoryExist children
  (childDirs <>) . mconcat <$> mapM listDirectoriesRecursive childDirs
