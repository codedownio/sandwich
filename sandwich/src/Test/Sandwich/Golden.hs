{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- This module is based on Test.Hspec.Golden from hspec-golden-0.2.0.0, which is MIT licensed.

module Test.Sandwich.Golden (
  -- * Main test function
  golden

  -- * Built-in Goldens.
  , goldenText
  , goldenString
  , goldenJSON
  , goldenShowable
  , mkGolden

  -- * Parameters for a 'Golden'.
  , goldenOutput
  , goldenWriteToFile
  , goldenReadFromFile
  , goldenFile
  , goldenActualFile
  , goldenFailFirstTime
  ) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
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
  goldenName :: String
  -- | Expected output.
  , goldenOutput :: a
  -- | How to write into the golden file the file.
  , goldenWriteToFile :: FilePath -> a -> IO ()
  -- | How to read the file.
  , goldenReadFromFile :: FilePath -> IO a
  -- | Where to read/write the golden file for this test.
  , goldenFile :: FilePath
  -- | Where to save the actual file for this test. If it is @Nothing@ then no file is written.
  , goldenActualFile :: Maybe FilePath
  -- | Whether to record a failure the first time this test is run.
  , goldenFailFirstTime :: Bool
  }


-- | Make your own 'Golden' constructor by providing 'goldenWriteToFile' and 'goldenReadFromFile'.
mkGolden :: (FilePath -> a -> IO ()) -> (FilePath -> IO a) -> String -> a -> Golden a
mkGolden goldenWriteToFile goldenReadFromFile name output = Golden {
  goldenName = name
  , goldenOutput = output
  , goldenWriteToFile = goldenWriteToFile
  , goldenReadFromFile = goldenReadFromFile
  , goldenFile = defaultDirGoldenTest </> name </> "golden"
  , goldenActualFile = Just (defaultDirGoldenTest </> name </> "actual")
  , goldenFailFirstTime = False
  }

-- | Golden for a 'T.Text'.
goldenText :: String -> T.Text -> Golden T.Text
goldenText = mkGolden T.writeFile T.readFile

-- | Golden for a 'String'.
goldenString :: String -> String -> Golden String
goldenString = mkGolden writeFile readFile

-- | Golden for an Aeson value ('ToJSON'/'FromJSON').
goldenJSON :: (A.ToJSON a, A.FromJSON a) => String -> a -> Golden a
goldenJSON = mkGolden (\f x -> BL.writeFile f $ A.encode x) $ \f ->
  eitherDecodeFileStrict' f >>= \case
    Left err -> expectationFailure [i|Failed to decode JSON value in #{f}: #{err}|]
    Right x -> return x

-- | Golden for a general 'Show'/'Read' type.
goldenShowable :: (Show a, Read a) => String -> a -> Golden a
goldenShowable = mkGolden (\f x -> writeFile f (show x)) ((read <$>) . readFile)

-- | Runs a Golden test.

golden :: (MonadIO m, MonadThrow m, Eq str, Show str) => Golden str -> Free (SpecCommand context m) ()
golden (Golden {..}) = it goldenName $ do
  let goldenTestDir = takeDirectory goldenFile
  liftIO $ createDirectoryIfMissing True goldenTestDir
  goldenFileExist <- liftIO $ doesFileExist goldenFile

  case goldenActualFile of
    Nothing -> return ()
    Just actual -> do
      -- It is recommended to always write the actual file
      let actualDir = takeDirectory actual
      liftIO $ createDirectoryIfMissing True actualDir
      liftIO $ goldenWriteToFile actual goldenOutput

  if not goldenFileExist
    then do
        liftIO $ goldenWriteToFile goldenFile goldenOutput
        when goldenFailFirstTime $ expectationFailure [i|Failed due to first execution and goldenFailFirstTime=True.|]
    else do
       liftIO (goldenReadFromFile goldenFile) >>= \case
         x | x == goldenOutput -> return ()
         x -> throwIO $ ExpectedButGot (Just callStack) (SEB x) (SEB goldenOutput)
