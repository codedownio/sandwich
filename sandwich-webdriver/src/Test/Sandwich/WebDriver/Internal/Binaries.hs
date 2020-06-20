{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, Rank2Types #-}

module Test.Sandwich.WebDriver.Internal.Binaries (
  downloadSeleniumIfNecessary
  , downloadChromeDriverIfNecessary
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process
import Test.Sandwich.WebDriver.Internal.Binaries.Util
import Test.Sandwich.WebDriver.Internal.Util

downloadSeleniumIfNecessary :: FilePath -> IO (Either T.Text FilePath)
downloadSeleniumIfNecessary toolsDir = leftOnException' $ do
  let seleniumPath = [i|#{toolsDir}/selenium-server.jar|]
  liftIO (doesFileExist seleniumPath >>= flip unless (downloadSelenium seleniumPath))
  return seleniumPath

downloadSelenium :: FilePath -> IO ()
downloadSelenium seleniumPath = void $ do
  putStrLn [i|Downloading selenium-server.jar to #{seleniumPath}|]
  createDirectoryIfMissing True (takeDirectory seleniumPath)
  readCreateProcess (shell [i|curl https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar -o #{seleniumPath}|]) ""

downloadChromeDriverIfNecessary :: FilePath -> IO (Either T.Text FilePath)
downloadChromeDriverIfNecessary toolsDir = runExceptT $ do
  chromeVersion <- ExceptT detectChromeVersion
  chromeDriverVersion@(w, x, y, z) <- ExceptT $ getChromeDriverVersion chromeVersion
  let downloadPath = getChromeDriverDownloadPath chromeDriverVersion detectPlatform

  let executableName = case detectPlatform of
        Windows -> "chromedriver.exe"
        _ -> "chromedriver"
  let chromeDriverPath = [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{executableName}|]
  (liftIO $ doesFileExist chromeDriverPath) >>= flip unless (ExceptT $ downloadAndUnzipToPath downloadPath chromeDriverPath)

  return chromeDriverPath

downloadAndUnzipToPath :: T.Text -> FilePath -> IO (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  putStrLn [i|Downloading #{downloadPath} to #{localPath}|]
  createDirectoryIfMissing True (takeDirectory localPath)
  void $ readCreateProcess (shell [i|wget -nc -O - #{downloadPath} | gunzip - > #{localPath}|]) ""
  void $ readCreateProcess (shell [i|chmod u+x #{localPath}|]) ""
