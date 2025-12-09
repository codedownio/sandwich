{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Test.Sandwich
import UnliftIO.Exception

fileContent = Label :: Label "fileContent" String

introduceFileContent = introduce "Introduce FileContent" fileContent (liftIO $ readFile "demos/demo-context-simple/example") (const $ return ())

contextsDemo :: TopSpec
contextsDemo = describe "Contexts" $ do
  describe "with fileContent of `example`" $ do
    introduceFileContent $ do
      it "`example` does contain `0.0.1`" $ do
        fc <- getContext fileContent
        fc `shouldBe`"0.0.1\n"


testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextsDemo
