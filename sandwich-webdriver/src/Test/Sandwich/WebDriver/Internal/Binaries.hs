{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, Rank2Types #-}

module Test.Sandwich.WebDriver.Internal.Binaries (
  obtainSelenium
  , obtainChromeDriver
  , obtainGeckoDriver
  , downloadSeleniumIfNecessary
  , downloadChromeDriverIfNecessary
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import System.FilePath
import System.Process
import Test.Sandwich.Logging
import Test.Sandwich.WebDriver.Internal.Binaries.Util
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util

type Constraints m = (HasCallStack, MonadLogger m, MonadIO m, MonadBaseControl IO m)

-- * Obtaining binaries

  -- TODO: remove curl dependencies here

obtainSelenium :: (MonadIO m, MonadLogger m) => FilePath -> SeleniumToUse -> m (Either T.Text FilePath)
obtainSelenium toolsDir (DownloadSeleniumFrom url) = do
  let seleniumPath = [i|#{toolsDir}/selenium-server-standalone.jar|]
  liftIO $ createDirectoryIfMissing True (takeDirectory seleniumPath)
  unlessM (liftIO $ doesFileExist seleniumPath) $
    void $ liftIO $ readCreateProcess (shell [i|curl #{url} -o #{seleniumPath}|]) ""
  return $ Right seleniumPath
obtainSelenium toolsDir DownloadSeleniumDefault = do
  let seleniumPath = [i|#{toolsDir}/selenium-server-standalone-3.141.59.jar|]
  liftIO $ createDirectoryIfMissing True (takeDirectory seleniumPath)
  unlessM (liftIO $ doesFileExist seleniumPath) $
    void $ liftIO $ readCreateProcess (shell [i|curl https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar -o #{seleniumPath}|]) ""
  return $ Right seleniumPath
obtainSelenium _ (UseSeleniumAt path) =
  (liftIO $ doesFileExist path) >>= \case
    False -> return $ Left [i|Path '#{path}' didn't exist|]
    True -> return $ Right path

obtainChromeDriver :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => FilePath -> ChromeDriverToUse -> m (Either T.Text FilePath)
obtainChromeDriver toolsDir (DownloadChromeDriverFrom url) = do
  let path = [i|#{toolsDir}/#{chromeDriverExecutable}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  unlessM (liftIO $ doesFileExist path) $
    void $ liftIO $ readCreateProcess (shell [i|curl #{url} -o #{path}|]) ""
  return $ Right path
obtainChromeDriver toolsDir (DownloadChromeDriverVersion chromeDriverVersion) = runExceptT $ do
  let path = getChromeDriverPath toolsDir chromeDriverVersion
  (liftIO $ doesFileExist path) >>= \case
    True -> return path
    False -> do
      let downloadPath = getChromeDriverDownloadUrl chromeDriverVersion detectPlatform
      ExceptT $ downloadAndUnzipToPath downloadPath path
      return path
obtainChromeDriver toolsDir DownloadChromeDriverAutodetect = runExceptT $ do
  version <- ExceptT $ liftIO getChromeDriverVersion
  ExceptT $ obtainChromeDriver toolsDir (DownloadChromeDriverVersion version)
obtainChromeDriver _ (UseChromeDriverAt path) =
  (liftIO $ doesFileExist path) >>= \case
    False -> return $ Left [i|Path '#{path}' didn't exist|]
    True -> return $ Right path

obtainGeckoDriver :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => FilePath -> GeckoDriverToUse -> m (Either T.Text FilePath)
obtainGeckoDriver toolsDir (DownloadGeckoDriverFrom url) = do
  let path = [i|#{toolsDir}/#{geckoDriverExecutable}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  unlessM (liftIO $ doesFileExist path) $
    void $ liftIO $ readCreateProcess (shell [i|curl #{url} -o #{path}|]) ""
  return $ Right path
obtainGeckoDriver toolsDir (DownloadGeckoDriverVersion geckoDriverVersion) = runExceptT $ do
  let path = getGeckoDriverPath toolsDir geckoDriverVersion
  (liftIO $ doesFileExist path) >>= \case
    True -> return path
    False -> do
      let downloadPath = getGeckoDriverDownloadUrl geckoDriverVersion detectPlatform
      ExceptT $ downloadAndUntarballToPath downloadPath path
      return path
obtainGeckoDriver toolsDir DownloadGeckoDriverAutodetect = runExceptT $ do
  version <- ExceptT $ liftIO getGeckoDriverVersion
  ExceptT $ obtainGeckoDriver toolsDir (DownloadGeckoDriverVersion version)
obtainGeckoDriver _ (UseGeckoDriverAt path) =
  (liftIO $ doesFileExist path) >>= \case
    False -> return $ Left [i|Path '#{path}' didn't exist|]
    True -> return $ Right path

-- * Lower level helpers


downloadSeleniumIfNecessary :: Constraints m => FilePath -> m (Either T.Text FilePath)
downloadSeleniumIfNecessary toolsDir = leftOnException' $ do
  let seleniumPath = [i|#{toolsDir}/selenium-server.jar|]
  (liftIO (doesFileExist seleniumPath) >>= flip unless (downloadSelenium seleniumPath))
  return seleniumPath

downloadSelenium :: Constraints m => FilePath -> m ()
downloadSelenium seleniumPath = void $ do
  info [i|Downloading selenium-server.jar to #{seleniumPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory seleniumPath)
  liftIO $ readCreateProcess (shell [i|curl https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar -o #{seleniumPath}|]) ""

downloadChromeDriverIfNecessary' :: Constraints m => FilePath -> ChromeDriverVersion -> m (Either T.Text FilePath)
downloadChromeDriverIfNecessary' toolsDir chromeDriverVersion = runExceptT $ do
  let chromeDriverPath = getChromeDriverPath toolsDir chromeDriverVersion

  unlessM (liftIO $ doesFileExist chromeDriverPath) $ do
    let downloadPath = getChromeDriverDownloadUrl chromeDriverVersion detectPlatform
    ExceptT $ downloadAndUnzipToPath downloadPath chromeDriverPath

  return chromeDriverPath

downloadChromeDriverIfNecessary :: Constraints m => FilePath -> m (Either T.Text FilePath)
downloadChromeDriverIfNecessary toolsDir = runExceptT $ do
  chromeDriverVersion <- ExceptT $ liftIO getChromeDriverVersion
  ExceptT $ downloadChromeDriverIfNecessary' toolsDir chromeDriverVersion

getChromeDriverPath :: FilePath -> ChromeDriverVersion -> FilePath
getChromeDriverPath toolsDir (ChromeDriverVersion (w, x, y, z)) = [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{chromeDriverExecutable}|]

getGeckoDriverPath :: FilePath -> GeckoDriverVersion -> FilePath
getGeckoDriverPath toolsDir (GeckoDriverVersion (x, y, z)) = [i|#{toolsDir}/geckodrivers/#{x}.#{y}.#{z}/#{geckoDriverExecutable}|]

chromeDriverExecutable = case detectPlatform of
  Windows -> "chromedriver.exe"
  _ -> "chromedriver"

geckoDriverExecutable = case detectPlatform of
  Windows -> "geckodriver.exe"
  _ -> "geckodriver"

downloadAndUnzipToPath :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  liftIO $ void $ readCreateProcess (shell [i|wget -nc -O - #{downloadPath} | gunzip - > #{localPath}|]) ""
  liftIO $ void $ readCreateProcess (shell [i|chmod u+x #{localPath}|]) ""

downloadAndUntarballToPath :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUntarballToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  liftIO $ void $ readCreateProcess (shell [i|wget -qO- #{downloadPath} | tar xvz  -C #{takeDirectory localPath}|]) ""
  liftIO $ void $ readCreateProcess (shell [i|chmod u+x #{localPath}|]) ""

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (\t -> unless t s)
