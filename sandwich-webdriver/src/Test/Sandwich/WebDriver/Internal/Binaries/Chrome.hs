{-# LANGUAGE ConstraintKinds #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Chrome (
  ChromeToUse(..)
  , ChromeDriverToUse(..)
  , ChromeVersion(..)
  , ChromeDriverVersion(..)

  , obtainChromeDriver
  , downloadChromeDriverIfNecessary
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Data.String.Interpolate
import Data.Text as T
import GHC.Stack
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

-- | Manually obtain a chromedriver binary, according to the 'ChromeDriverToUse' policy,
-- storing it under the provided 'FilePath' if necessary and returning the exact path.
obtainChromeDriver :: (
  MonadUnliftIO m, MonadLogger m
  ) => FilePath -> ChromeDriverToUse -> m (Either T.Text FilePath)
obtainChromeDriver toolsDir (DownloadChromeDriverFrom url) = do
  let path = [i|#{toolsDir}/#{chromeDriverExecutable}|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath url path
  return $ Right path
obtainChromeDriver toolsDir (DownloadChromeDriverVersion chromeDriverVersion) = runExceptT $ do
  let path = getChromeDriverPath toolsDir chromeDriverVersion
  liftIO (doesFileExist path) >>= \case
    True -> return path
    False -> do
      let downloadPath = getChromeDriverDownloadUrl chromeDriverVersion detectPlatform
      ExceptT $ downloadAndUnzipToPath downloadPath path
      return path
obtainChromeDriver toolsDir (DownloadChromeDriverAutodetect chromePath) = runExceptT $ do
  version <- ExceptT $ liftIO $ getChromeDriverVersion chromePath
  ExceptT $ obtainChromeDriver toolsDir (DownloadChromeDriverVersion version)
obtainChromeDriver _ (UseChromeDriverAt path) = liftIO (doesFileExist path) >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> return $ Right path
obtainChromeDriver _ (UseChromeDriverFromNixpkgs nixContext) = undefined


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
