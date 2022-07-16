{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

{-| This module is based on Test.Hspec.Golden from hspec-golden-0.2.0.0, which is MIT licensed -}

module Test.Sandwich.Golden (
  golden'

  , defaultGolden
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
import GHC.Stack
import System.Directory
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Types.Spec


-- | Golden tests parameters
--
-- @
-- import           Data.Text (Text)
-- import qualified Data.Text.IO as T
--
-- goldenText :: String -> Text -> Golden Text
-- goldenText name actualOutput =
--   Golden {
--     output = actualOutput,
--     encodePretty = prettyText,
--     writeToFile = T.writeFile,
--     readFromFile = T.readFile,
--     goldenFile = ".specific-golden-dir" </> name </> "golden",
--     actualFile = Just (".specific-golden-dir" </> name </> "actual"),
--     failFirstTime = False
--   }
--
-- describe "myTextFunc" $
--   it "generates the right output with the right params" $
--     goldenText "myTextFunc" (myTextFunc params)
-- @

data Golden a = Golden {
  -- | Name
  name :: a
  -- | Output
  , output :: a
  -- | Makes the comparison pretty when the test fails
  , encodePretty :: a -> String
  -- | How to write into the golden file the file
  , writeToFile :: FilePath -> a -> IO ()
  -- | How to read the file
  , readFromFile :: FilePath -> IO a
  -- | Where to read/write the golden file for this test.
  , goldenFile :: FilePath
  -- | Where to save the actual file for this test. If it is @Nothing@ then no file is written.
  , actualFile :: Maybe FilePath
  -- | Whether to record a failure the first time this test is run
  , failFirstTime :: Bool
  }

-- fromGoldenResult :: GoldenResult -> FailureReason
-- fromGoldenResult SameOutput = Result "Golden and Actual output hasn't changed" Success
-- fromGoldenResult FirstExecutionSucceed  = Result "First time execution. Golden file created." Success
-- fromGoldenResult FirstExecutionFail =
--   Result "First time execution. Golden file created."
--          (Failure Nothing (Reason "failFirstTime is set to True"))
-- fromGoldenResult (MissmatchOutput expected actual) =
--   Result "Files golden and actual not match"
--          (Failure Nothing (ExpectedButGot Nothing expected actual))

-- | An example of Golden tests which output is 'String'
--
-- @
--  describe "html" $ do
--    it "generates html" $
--      defaultGolden "html" someHtml
-- @

defaultGolden :: String -> String -> Golden String
defaultGolden name output_ = Golden {
  name = name,
  output = output_,
  encodePretty = show,
  writeToFile = writeFile,
  readFromFile = readFile,
  goldenFile = ".golden" </> name </> "golden",
  actualFile = Just (".golden" </> name </> "actual"),
  failFirstTime = False
  }

-- | Runs a Golden test.

golden' :: (MonadIO m, MonadThrow m, Eq str, Show str) => Golden str -> Free (SpecCommand context m) ()
golden' (Golden {..}) = it (show name) $ do
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
        when failFirstTime $ expectationFailure [i|Failed due to first execution.|]
    else do
       liftIO (readFromFile goldenFile) >>= \case
         x | x == output -> return ()
         x -> throwIO $ ExpectedButGot (Just callStack) (SEB x) (SEB output)
