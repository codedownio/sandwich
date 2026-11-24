{-# LANGUAGE DataKinds #-}

module Main where

import qualified Around
import qualified Before
import qualified CancelOnLongExecution
import qualified Describe
import qualified EscapedExceptions
import qualified FolderPaths
import qualified Introduce
import qualified IntroduceWith
import qualified UnicodeLabels
import TestUtil


main :: IO ()
main = do
  testRunsDir <- getTestRunsDir
  mainWith $ do
    Around.tests
    Before.tests
    CancelOnLongExecution.tests
    Describe.tests
    EscapedExceptions.tests testRunsDir
    FolderPaths.tests
    Introduce.tests
    IntroduceWith.tests
    UnicodeLabels.tests testRunsDir
