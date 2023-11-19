{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.String.Interpolate
import Test.Sandwich

data DatabaseContext = MySQLDatabaseContext | SqliteDatabaseContext
  deriving Show

database = Label :: Label "database" DatabaseContext

setupDatabase :: MonadIO m => ExampleT context m DatabaseContext
setupDatabase = debug "Spinning up DB..." >> p 3 >> return MySQLDatabaseContext

teardownDatabase :: MonadIO m => DatabaseContext -> ExampleT context m ()
teardownDatabase db = debug "Tearing down DB..." >> p 2 >> return ()

introduceDatabase = introduceWith "introduceWith database" database $ \action ->
  bracket setupDatabase teardownDatabase (void . action)

basic :: TopSpec
basic = describe "Simple tests" $ do
  before "Pauses before" (p 3) $ do
    it "adds" $ (2 + 2) `shouldBe` 4

  after "Pauses after" (p 60) $ do
    it "adds" $ (2 + 2) `shouldBe` 4

  introduceDatabase $ do
    it "Uses the database" $ do
      db <- getContext database
      info [i|Got database: '#{db}'|]

  introduce "introduce database" database setupDatabase teardownDatabase $ do
    it "Uses the database" $ do
      db <- getContext database
      info [i|Got database: '#{db}'|]


p :: (MonadIO m) => Double -> m ()
p = liftIO . threadDelay . round . (* 1000000.0)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
