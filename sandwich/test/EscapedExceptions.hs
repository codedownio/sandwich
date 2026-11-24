{-# LANGUAGE DataKinds #-}

module EscapedExceptions (
  tests
  , main
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.String.Interpolate
import GHC.Stack
import System.Directory
import System.FilePath
import System.Timeout (timeout)
import Test.Sandwich
import Test.Sandwich.Internal
import TestUtil
import UnliftIO.Exception


tests :: MonadIO m => FilePath -> WriterT [SomeException] m ()
tests testRunsDir = do
  run (siblingsFinishWhenExceptionEscapesNode testRunsDir)
  run formatterFinalizeFailureFailsRun

main :: IO ()
-- Not point-free (mainWith . tests), since GHC 9.0 rejects that for mainWith's constrained argument
main = getTestRunsDir >>= \testRunsDir -> mainWith (tests testRunsDir)

-- * Tests

-- | An exception escaping a node's runner (here, from writing its failure.txt) shouldn't leave the
-- nodes after it unfinished, since anything waiting on them would wait forever.
siblingsFinishWhenExceptionEscapesNode :: (HasCallStack) => FilePath -> IO ()
siblingsFinishWhenExceptionEscapesNode testRunsDir = withTempArtifactsDir testRunsDir $ \dir -> do
  rts <- startSandwichTree (defaultOptions { optionsTestArtifactsDirectory = TestArtifactsFixedDirectory dir }) $
    describe "outer" $ do
      it "blocks its own failure.txt" $ do
        getCurrentFolder >>= \case
          Nothing -> expectationFailure "Expected a test folder"
          Just folder -> liftIO $ createDirectoryIfMissing True (folder </> "failure.txt")
        expectationFailure "Failing so sandwich writes failure.txt"
      it "comes after" $ return ()
  mapM_ waitForTree rts

  waitForAllNodesDone 10_000_000 rts >>= \case
    [] -> return ()
    notDone -> expectationFailure [i|Nodes never finished: #{notDone}|]

-- | A formatter that throws while finalizing should fail the run with its exception, not hang it.
formatterFinalizeFailureFailsRun :: (HasCallStack) => IO ()
formatterFinalizeFailureFailsRun = do
  result <- timeout 30_000_000 $ tryAny $ runSandwich' Nothing (defaultOptions { optionsFormatters = [SomeFormatter ThrowingFormatter] }) $
    it "passes" $ return ()

  case result of
    Nothing -> expectationFailure "Run didn't finish"
    Just (Right x) -> expectationFailure [i|Expected the formatter's exception, but the run returned #{x}|]
    Just (Left e) -> show e `mustBe` show finalizeError

data ThrowingFormatter = ThrowingFormatter deriving Show

instance Formatter ThrowingFormatter where
  formatterName _ = "throwing-formatter"
  runFormatter _ _ _ _ = return ()
  finalizeFormatter _ _ _ = liftIO $ throwIO finalizeError

finalizeError :: IOError
finalizeError = userError "Formatter failed to finalize"
