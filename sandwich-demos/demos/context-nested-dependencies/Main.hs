{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Common
import Control.Exception.Lifted
import Control.Monad
import Data.String.Interpolate
import Data.Time.Clock
import Test.Sandwich


-- * Database

data DatabaseContext = MySQLDatabaseContext | SqliteDatabaseContext
  deriving Show
database = Label :: Label "database" DatabaseContext
type HasDatabase context = HasLabel context "database" DatabaseContext

introduceDatabase = introduceWith "Introduce database" database $ \action ->
  bracket (debug "Spinning up DB..." >> return MySQLDatabaseContext)
          (\db -> debug "Tearing down DB..." >> return ())
          (void . action)

-- * Server

data Server = Server DatabaseContext deriving Show
server = Label :: Label "server" Server

introduceServer = introduceWith "Introduce server" server $ \action -> do
  bracket (do
              db <- getContext database
              debug "Spinning up server..."
              return $ Server db
          )
          (\server -> debug "Tearing down server..." >> return ())
          (void . action)

-- * Tests

contextNestedDepsDemo :: TopSpec
contextNestedDepsDemo = describe "Nested dependencies" $ do
  introduceDatabase $
    introduceServer $
      it "uses the server" $ do
        s <- getContext server
        debug [i|Got server: #{s}|]

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextNestedDepsDemo
