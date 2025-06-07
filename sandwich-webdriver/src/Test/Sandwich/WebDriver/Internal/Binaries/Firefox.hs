{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Firefox (
  obtainFirefox
  , obtainGeckoDriver

  , FirefoxToUse(..)
  , GeckoDriverToUse(..)
  , GeckoDriverVersion(..)
  ) where

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
  , MonadUnliftIO m, MonadLogger m
  ) => FirefoxToUse -> m (Either T.Text FilePath)
obtainFirefox UseFirefoxFromPath = do
  findExecutable "firefox" >>= \case
    Just p -> do
      debug [i|Found firefox: #{p}|]
      return $ Right p
    Nothing -> expectationFailure [i|Couldn't find "firefox" on the PATH|]
obtainFirefox (UseFirefoxAt p) = doesFileExist p >>= \case
  False -> return $ Left [i|Path '#{p}' didn't exist|]
  True -> do
    debug [i|Found firefox: #{p}|]
    return $ Right p
obtainFirefox (UseFirefoxFromNixpkgs nixContext) = do
  debug [i|Building Firefox with Nix|]
  -- ret <- case os of
  --   "darwin" ->
  --     -- The only Firefox version that currently works on Darwin as of 5/5/2025 is firefox-bin
  --     buildNixSymlinkJoin' nixContext ["firefox-bin"] >>= (liftIO . defaultFindFile "firefox")
  --   _ ->
  --     getBinaryViaNixPackage' @"firefox" nixContext "firefox"
  ret <- getBinaryViaNixPackage' @"firefox" nixContext "firefox"
  debug [i|Built Firefox: #{ret}|]
  return $ Right ret

-- | Manually obtain a @geckodriver@ binary, according to the 'GeckoDriverToUse' policy,
-- storing it under the provided 'FilePath' if necessary and returning the exact path.
obtainGeckoDriver :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLogger m
  )
  -- | How to obtain @geckodriver@
  => GeckoDriverToUse
  -> m (Either T.Text FilePath)
obtainGeckoDriver (DownloadGeckoDriverFrom toolsDir url) = do
  let path = [i|#{toolsDir}/#{geckoDriverExecutable}|]
  doesFileExist path >>= \case
    True -> do
      debug [i|GeckoDriver already existed at #{path}|]
    False -> do
      debug [i|Downloading GeckoDriver from #{url}...|]
      curlDownloadToPath url path
      debug [i|Downloaded GeckoDriver to #{path}|]
  return $ Right path
obtainGeckoDriver (DownloadGeckoDriverVersion toolsDir geckoDriverVersion) = runExceptT $ do
  let path = getGeckoDriverPath toolsDir geckoDriverVersion
  doesFileExist path >>= \case
    True -> do
      debug [i|GeckoDriver already existed at #{path}|]
      return path
    False -> do
      let downloadUrl = getGeckoDriverDownloadUrl geckoDriverVersion detectPlatform
      debug [i|Downloading GeckoDriver from #{downloadUrl}|]
      ExceptT $ downloadAndUntarballToPath downloadUrl path
      debug [i|Downloaded GeckoDriver to #{path}|]
      return path
obtainGeckoDriver (DownloadGeckoDriverAutodetect toolsDir) = runExceptT $ do
  version <- ExceptT $ liftIO $ getGeckoDriverVersion Nothing
  debug [i|Trying to obtain GeckoDriver version #{version} (autodetected)|]
  ExceptT $ obtainGeckoDriver (DownloadGeckoDriverVersion toolsDir version)
obtainGeckoDriver (UseGeckoDriverAt path) = liftIO (doesFileExist path) >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> do
    debug [i|Found GeckoDriver at #{path}|]
    return $ Right path
obtainGeckoDriver (UseGeckoDriverFromNixpkgs nixContext) = Right <$> getBinaryViaNixPackage' @"geckodriver" nixContext "geckodriver"


getGeckoDriverPath :: FilePath -> GeckoDriverVersion -> FilePath
getGeckoDriverPath toolsDir (GeckoDriverVersion (x, y, z)) = [i|#{toolsDir}/geckodrivers/#{x}.#{y}.#{z}/#{geckoDriverExecutable}|]

geckoDriverExecutable :: T.Text
geckoDriverExecutable = case detectPlatform of
  Windows -> "geckodriver.exe"
  _ -> "geckodriver"
