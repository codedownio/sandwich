{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, Rank2Types #-}

module Test.Sandwich.WebDriver.Internal.Binaries (
  downloadSeleniumIfNecessary
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
import Test.Sandwich.WebDriver.Internal.Util

type Constraints m = (HasCallStack, MonadLogger m, MonadIO m, MonadBaseControl IO m)

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

downloadChromeDriverIfNecessary :: Constraints m => FilePath -> m (Either T.Text FilePath)
downloadChromeDriverIfNecessary toolsDir = runExceptT $ do
  chromeVersion <- ExceptT $ liftIO detectChromeVersion
  chromeDriverVersion@(w, x, y, z) <- ExceptT $ liftIO $ getChromeDriverVersion chromeVersion
  let downloadPath = getChromeDriverDownloadPath chromeDriverVersion detectPlatform

  let executableName = case detectPlatform of
        Windows -> "chromedriver.exe"
        _ -> "chromedriver"
  let chromeDriverPath = [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{executableName}|]
  (liftIO $ doesFileExist chromeDriverPath) >>= flip unless (ExceptT $ downloadAndUnzipToPath downloadPath chromeDriverPath)

  return chromeDriverPath

downloadAndUnzipToPath :: Constraints m => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  liftIO $ void $ readCreateProcess (shell [i|wget -nc -O - #{downloadPath} | gunzip - > #{localPath}|]) ""
  liftIO $ void $ readCreateProcess (shell [i|chmod u+x #{localPath}|]) ""
