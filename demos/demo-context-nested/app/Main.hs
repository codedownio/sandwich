{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Common
import Control.Monad
import Control.Monad.IO.Unlift
import Data.String.Interpolate
import Test.Sandwich
import UnliftIO.Exception

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

introduceServer :: (HasDatabase context, MonadUnliftIO m)
  => SpecFree (LabelValue "server" Server :> context) m () -> SpecFree context m ()
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
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions contextNestedDepsDemo
