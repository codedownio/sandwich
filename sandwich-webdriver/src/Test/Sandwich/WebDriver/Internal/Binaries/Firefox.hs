{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Firefox where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.WebDriver.Internal.Binaries.Common
import Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Detect
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Types
import UnliftIO.Directory


-- | Manually obtain a firefox binary, according to the 'FirefoxToUse' policy,
obtainFirefox :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => FirefoxToUse -> m (Either T.Text FilePath)
obtainFirefox UseFirefoxFromPath = do
  findExecutable "firefox" >>= \case
    Just p -> return $ Right p
    Nothing -> expectationFailure [i|Couldn't find "firefox" on the PATH|]
obtainFirefox (UseFirefoxAt p) = doesFileExist p >>= \case
  False -> return $ Left [i|Path '#{p}' didn't exist|]
  True -> return $ Right p
obtainFirefox (UseFirefoxFromNixpkgs nixContext) =
  Right <$> getBinaryViaNixPackage' @"firefox" nixContext "firefox"

-- | Manually obtain a geckodriver binary, according to the 'GeckoDriverToUse' policy,
-- storing it under the provided 'FilePath' if necessary and returning the exact path.
obtainGeckoDriver :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => GeckoDriverToUse -> m (Either T.Text FilePath)
obtainGeckoDriver (DownloadGeckoDriverFrom toolsDir url) = do
  let path = [i|#{toolsDir}/#{geckoDriverExecutable}|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath url path
  return $ Right path
obtainGeckoDriver (DownloadGeckoDriverVersion toolsDir geckoDriverVersion) = runExceptT $ do
  let path = getGeckoDriverPath toolsDir geckoDriverVersion
  liftIO (doesFileExist path) >>= \case
    True -> return path
    False -> do
      let downloadPath = getGeckoDriverDownloadUrl geckoDriverVersion detectPlatform
      ExceptT $ downloadAndUntarballToPath downloadPath path
      return path
obtainGeckoDriver (DownloadGeckoDriverAutodetect toolsDir) = runExceptT $ do
  version <- ExceptT $ liftIO $ getGeckoDriverVersion Nothing
  ExceptT $ obtainGeckoDriver (DownloadGeckoDriverVersion toolsDir version)
obtainGeckoDriver (UseGeckoDriverAt path) = liftIO (doesFileExist path) >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> return $ Right path
obtainGeckoDriver (UseGeckoDriverFromNixpkgs nixContext) = Right <$> getBinaryViaNixPackage' @"geckodriver" nixContext "geckodriver"


getGeckoDriverPath :: FilePath -> GeckoDriverVersion -> FilePath
getGeckoDriverPath toolsDir (GeckoDriverVersion (x, y, z)) = [i|#{toolsDir}/geckodrivers/#{x}.#{y}.#{z}/#{geckoDriverExecutable}|]

geckoDriverExecutable :: T.Text
geckoDriverExecutable = case detectPlatform of
  Windows -> "geckodriver.exe"
  _ -> "geckodriver"
