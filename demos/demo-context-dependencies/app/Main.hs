{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

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

type HasDatabase context = HasLabel context "database" DatabaseContext

type DatabaseSpec = forall context. (HasDatabase context) => SpecFree context IO ()

contextDepsDemo :: TopSpec
contextDepsDemo = describe "Context dependencies" $ do
  introduceDatabase $ do
    databaseTest1
    databaseTest2

contextDepsDemo2 :: TopSpec
contextDepsDemo2 = describe "Context dependencies" $ do
  introduceDatabase databaseTest1
  introduceDatabase databaseTest2

databaseTest1 :: DatabaseSpec
databaseTest1 = do
  it "uses the database 1" $ getContext database >>= \db -> info [i|Got database: '#{db}'|]

databaseTest2 :: DatabaseSpec
databaseTest2 = do
  it "uses the database 2" $ getContext database >>= \db -> info [i|Got database: '#{db}'|]

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextDepsDemo
