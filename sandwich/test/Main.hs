{-# LANGUAGE DataKinds #-}

module Main where

import qualified Around
import qualified Before
import qualified CancelOnLongExecution
import qualified Describe
import qualified FolderPaths
import qualified Introduce
import qualified IntroduceWith
import TestUtil


main :: IO ()
main = mainWith $ do
  Around.tests
  Before.tests
  CancelOnLongExecution.tests
  Describe.tests
  FolderPaths.tests
  Introduce.tests
  IntroduceWith.tests
