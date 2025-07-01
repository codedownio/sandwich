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

-- introduceDatabaseAllocFailure = introduceWith "Introduce database" database $ \action ->
--   bracket (debug "Spinning up DB..." >> expectationFailure "Alloc failure" >> return MySQLDatabaseContext)
--           (\db -> debug "Tearing down DB..." >> return ())
--           (void . action)

introduceDatabaseCleanupFailure = introduceWith "Introduce database" database $ \action ->
  bracket (debug "Spinning up DB..." >> return MySQLDatabaseContext)
          (\db -> do
              debug "Tearing down DB..."
              expectationFailure "Cleanup failure"
          )
          (void . action)

contextsDemo :: TopSpec
contextsDemo = describe "Contexts" $ do
  -- introduceDatabaseAllocFailure $ do
  introduceDatabaseCleanupFailure $ do
    it "Uses the database 1" $ getContext database >>= \db -> info [i|Got database: '#{db}'|]
    it "Uses the database 2" $ getContext database >>= \db -> info [i|Got database: '#{db}'|]
    it "Uses the database 3" $ getContext database >>= \db -> info [i|Got database: '#{db}'|]


testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextsDemo
