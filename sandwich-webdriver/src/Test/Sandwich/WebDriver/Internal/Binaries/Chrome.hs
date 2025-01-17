{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Chrome (
  obtainChrome
  , obtainChromeDriver

  -- * Lower-level
  , downloadChromeDriverIfNecessary

  -- * Types
  , ChromeToUse(..)
  , ChromeDriverToUse(..)
  , ChromeVersion(..)
  , ChromeDriverVersion(..)
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.String.Interpolate
import Data.Text as T
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome.Detect
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome.Types
import Test.Sandwich.WebDriver.Internal.Binaries.Common
import Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform
import UnliftIO.Directory


type Constraints m = (
  HasCallStack
  , MonadLogger m
  , MonadUnliftIO m
  )

-- | Manually obtain a chrome binary, according to the 'ChromeToUse' policy,
obtainChrome :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLogger m
  ) => ChromeToUse -> m (Either T.Text FilePath)
obtainChrome UseChromeFromPath = do
  findExecutable "google-chrome" >>= \case
    Just p -> do
      debug [i|Found Chrome at #{p}|]
      return $ Right p
    Nothing -> findExecutable "google-chrome-stable" >>= \case
      Just p -> do
        debug [i|Found Chrome at #{p}|]
        return $ Right p
      Nothing -> expectationFailure [i|Couldn't find either "google-chrome" or "google-chrome-stable" on the PATH|]
obtainChrome (UseChromeAt p) = doesFileExist p >>= \case
  False -> return $ Left [i|Path '#{p}' didn't exist|]
  True -> do
    debug [i|Found Chrome at #{p}|]
    return $ Right p
obtainChrome (UseChromeFromNixpkgs nixContext) = do
  debug [i|Building Chrome with Nix...|]
  ret <- getBinaryViaNixPackage' @"google-chrome-stable" nixContext "google-chrome"
  debug [i|Built Chrome: #{ret}|]
  return $ Right ret

-- | Manually obtain a @chromedriver@ binary, according to the 'ChromeDriverToUse' policy.
obtainChromeDriver :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLogger m
  )
  -- | How to obtain @chromedriver@
  => ChromeDriverToUse
  -> m (Either T.Text FilePath)
obtainChromeDriver (DownloadChromeDriverFrom toolsDir url) = do
  let path = [i|#{toolsDir}/#{chromeDriverExecutable}|]
  doesFileExist path >>= \case
    True -> do
      debug [i|chromedriver already existed at #{path}|]
    False -> do
      debug [i|Downloading chromedriver from #{url}...|]
      curlDownloadToPath url path
      debug [i|Downloaded chromedriver to #{path}|]
  return $ Right path
obtainChromeDriver (DownloadChromeDriverVersion toolsDir chromeDriverVersion) = runExceptT $ do
  let path = getChromeDriverPath toolsDir chromeDriverVersion
  liftIO (doesFileExist path) >>= \case
    True -> do
      debug [i|Found chromedriver at #{path}|]
      return path
    False -> do
      let downloadUrl = getChromeDriverDownloadUrl chromeDriverVersion detectPlatform
      debug [i|Downloading chromedriver from #{downloadUrl}...|]
      ExceptT $ downloadAndUnzipToPath downloadUrl path
      debug [i|Downloaded chromedriver to #{path}|]
      return path
obtainChromeDriver (DownloadChromeDriverAutodetect toolsDir chromePath) = runExceptT $ do
  version <- ExceptT $ liftIO $ getChromeDriverVersion chromePath
  ExceptT $ obtainChromeDriver (DownloadChromeDriverVersion toolsDir version)
obtainChromeDriver (UseChromeDriverAt path) = doesFileExist path >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> do
    debug [i|Found chromedriver at #{path}|]
    return $ Right path
obtainChromeDriver (UseChromeDriverFromNixpkgs nixContext) = do
  debug [i|Building chromedriver with Nix|]
  ret <- getBinaryViaNixPackage' @"chromedriver" nixContext "chromedriver"
  debug [i|Built chromedriver: #{ret}|]
  return $ Right ret

downloadChromeDriverIfNecessary' :: Constraints m => FilePath -> ChromeDriverVersion -> m (Either T.Text FilePath)
downloadChromeDriverIfNecessary' toolsDir chromeDriverVersion = runExceptT $ do
  let chromeDriverPath = getChromeDriverPath toolsDir chromeDriverVersion

  unlessM (liftIO $ doesFileExist chromeDriverPath) $ do
    let downloadPath = getChromeDriverDownloadUrl chromeDriverVersion detectPlatform
    ExceptT $ downloadAndUnzipToPath downloadPath chromeDriverPath

  return chromeDriverPath

downloadChromeDriverIfNecessary :: Constraints m => FilePath -> FilePath -> m (Either T.Text FilePath)
downloadChromeDriverIfNecessary chromePath toolsDir = runExceptT $ do
  chromeDriverVersion <- ExceptT $ liftIO $ getChromeDriverVersion chromePath
  ExceptT $ downloadChromeDriverIfNecessary' toolsDir chromeDriverVersion

getChromeDriverPath :: FilePath -> ChromeDriverVersion -> FilePath
getChromeDriverPath toolsDir (ChromeDriverVersionTuple (w, x, y, z)) =
  [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{chromeDriverExecutable}|]
getChromeDriverPath toolsDir (ChromeDriverVersionExactUrl (w, x, y, z) _) =
  [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{chromeDriverExecutable}|]

chromeDriverExecutable :: T.Text
chromeDriverExecutable = case detectPlatform of
  Windows -> "chromedriver.exe"
  _ -> "chromedriver"
