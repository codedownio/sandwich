{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Control.Monad
import Data.String.Interpolate
import Test.Sandwich
import UnliftIO.Exception

data DatabaseContext = MySQLDatabaseContext | SqliteDatabaseContext
  deriving Show

database = Label :: Label "database" DatabaseContext

introduceDatabase = introduceWith "Introduce database" database $ \action ->
  bracket (debug "Spinning up DB..." >> return MySQLDatabaseContext)
          (\db -> debug "Tearing down DB..." >> return ())
          (void . action)

contextsDemo :: TopSpec
contextsDemo = describe "Contexts" $ do
  introduceDatabase $ do
    it "Uses the database" $ do
      db <- getContext database
      info [i|Got database: '#{db}'|]

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextsDemo
