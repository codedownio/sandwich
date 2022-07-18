{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

{-| This module is based on Test.Hspec.Golden from hspec-golden-0.2.0.0, which is MIT licensed. -}

module Test.Sandwich.Golden (
  -- * Main test function
  golden

  -- * Built-in Goldens.
  , goldenText
  , goldenString
  , goldenShowable
  , mkGolden

  -- * Parameters for a 'Golden'.
  , output
  , encodePretty
  , writeToFile
  , readFromFile
  , goldenFile
  , actualFile
  , failFirstTime
  ) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import System.Directory
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Golden.Update
import Test.Sandwich.Types.Spec


data Golden a = Golden {
  -- | Name
  name :: String
  -- | Expected output.
  , output :: a
  -- | Makes the comparison pretty when the test fails.
  , encodePretty :: a -> String
  -- | How to write into the golden file the file.
  , writeToFile :: FilePath -> a -> IO ()
  -- | How to read the file.
  , readFromFile :: FilePath -> IO a
  -- | Where to read/write the golden file for this test.
  , goldenFile :: FilePath
  -- | Where to save the actual file for this test. If it is @Nothing@ then no file is written.
  , actualFile :: Maybe FilePath
  -- | Whether to record a failure the first time this test is run.
  , failFirstTime :: Bool
  }


-- | Golden functions

-- | Make your own 'Golden' by providing 'encodePretty', 'writeToFile', and 'readFromFile'.
mkGolden :: (a -> String) -> (FilePath -> a -> IO ()) -> (FilePath -> IO a) -> String -> a -> Golden a
mkGolden encodePretty writeToFile readFromFile name output = Golden {
  name = name
  , output = output
  , encodePretty = encodePretty
  , writeToFile = writeToFile
  , readFromFile = readFromFile
  , goldenFile = defaultDirGoldenTest </> name </> "golden"
  , actualFile = Just (defaultDirGoldenTest </> name </> "actual")
  , failFirstTime = False
  }

-- | Golden for a 'T.Text'.
goldenText :: String -> T.Text -> Golden T.Text
goldenText = mkGolden T.unpack T.writeFile T.readFile

-- | Golden for a 'String'.
goldenString :: String -> String -> Golden String
goldenString = mkGolden show writeFile readFile

-- | Golden for a general 'Show'/'Read' type.
goldenShowable :: (Show a, Read a) => String -> a -> Golden a
goldenShowable = mkGolden show (\f x -> writeFile f (show x)) ((read <$>) . readFile)

-- | Runs a Golden test.

golden :: (MonadIO m, MonadThrow m, Eq str, Show str) => Golden str -> Free (SpecCommand context m) ()
golden (Golden {..}) = it (show name) $ do
  let goldenTestDir = takeDirectory goldenFile
  liftIO $ createDirectoryIfMissing True goldenTestDir
  goldenFileExist <- liftIO $ doesFileExist goldenFile

  case actualFile of
    Nothing -> return ()
    Just actual -> do
      -- It is recommended to always write the actual file
      let actualDir = takeDirectory actual
      liftIO $ createDirectoryIfMissing True actualDir
      liftIO $ writeToFile actual output

  if not goldenFileExist
    then do
        liftIO $ writeToFile goldenFile output
        when failFirstTime $ expectationFailure [i|Failed due to first execution and failFirstTime=True.|]
    else do
       liftIO (readFromFile goldenFile) >>= \case
         x | x == output -> return ()
         x -> throwIO $ ExpectedButGot (Just callStack) (SEB x) (SEB output)
