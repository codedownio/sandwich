{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.String.Interpolate
import Test.Sandwich
import UnliftIO.Exception


-- * Mock database

data DatabaseContext = MySQLDatabaseContext | SqliteDatabaseContext
  deriving Show

database = Label :: Label "database" DatabaseContext

introduceDatabase = introduce "Introduce database" database
  (debug "Spinning up DB..." >> return MySQLDatabaseContext)
  (\db -> debug "Tearing down DB..." >> return ())

-- * IO example

fileContents = Label :: Label "fileContents" String

introduceFileContents = introduce "Introduce file contents" fileContents
  (liftIO $ readFile "demos/demo-contexts/app/Main.hs")
  (const $ return ())

contextsDemo :: TopSpec
contextsDemo = describe "Contexts" $ do
  introduceDatabase $ do
    it "Uses the database" $ do
      db <- getContext database
      info [i|Got database: '#{db}'|]

      maybeDb <- getContextMaybe database
      info [i|Got database from getContextMaybe: #{maybeDb}|]

  introduceFileContents $ do
    it "Uses the file contents" $ do
      contents <- getContext fileContents
      info [i|Got fileContents (first 10 lines):\n\n#{L.unlines $ L.take 10 $ L.lines contents}|]

  it "Uses a maybe database" $ do
    maybeDb <- getContextMaybe database
    info [i|Got database from getContextMaybe: #{maybeDb}|]

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextsDemo
