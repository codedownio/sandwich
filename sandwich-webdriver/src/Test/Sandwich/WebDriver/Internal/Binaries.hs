{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Test.Sandwich.WebDriver.Internal.Binaries (
  obtainSelenium
  , obtainChromeDriver
  , obtainGeckoDriver
  , downloadSeleniumIfNecessary
  , downloadChromeDriverIfNecessary
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich.Expectations
import Test.Sandwich.Logging
import Test.Sandwich.WebDriver.Internal.Binaries.DetectChrome
import Test.Sandwich.WebDriver.Internal.Binaries.DetectFirefox
import Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import UnliftIO.Temporary


type Constraints m = (
  HasCallStack
  , MonadLogger m
  , MonadUnliftIO m
  )

-- * Obtaining binaries

defaultSeleniumJarUrl :: String
defaultSeleniumJarUrl = "https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar"

-- TODO: remove curl dependencies here

-- | Manually obtain a Selenium server JAR file, according to the 'SeleniumToUse' policy,
-- storing it under the provided 'FilePath' if necessary and returning the exact path.
obtainSelenium :: (MonadUnliftIO m, MonadLogger m) => FilePath -> SeleniumToUse -> m (Either T.Text FilePath)
obtainSelenium toolsDir (DownloadSeleniumFrom url) = do
  let path = [i|#{toolsDir}/selenium-server-standalone.jar|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath url path
  return $ Right path
obtainSelenium toolsDir DownloadSeleniumDefault = do
  let path = [i|#{toolsDir}/selenium-server-standalone-3.141.59.jar|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath defaultSeleniumJarUrl path
  return $ Right path
obtainSelenium _ (UseSeleniumAt path) = liftIO (doesFileExist path) >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> return $ Right path

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

-- | Manually obtain a geckodriver binary, according to the 'GeckoDriverToUse' policy,
-- storing it under the provided 'FilePath' if necessary and returning the exact path.
obtainGeckoDriver :: (MonadUnliftIO m, MonadLogger m) => FilePath -> GeckoDriverToUse -> m (Either T.Text FilePath)
obtainGeckoDriver toolsDir (DownloadGeckoDriverFrom url) = do
  let path = [i|#{toolsDir}/#{geckoDriverExecutable}|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath url path
  return $ Right path
obtainGeckoDriver toolsDir (DownloadGeckoDriverVersion geckoDriverVersion) = runExceptT $ do
  let path = getGeckoDriverPath toolsDir geckoDriverVersion
  liftIO (doesFileExist path) >>= \case
    True -> return path
    False -> do
      let downloadPath = getGeckoDriverDownloadUrl geckoDriverVersion detectPlatform
      ExceptT $ downloadAndUntarballToPath downloadPath path
      return path
obtainGeckoDriver toolsDir (DownloadGeckoDriverAutodetect maybeFirefoxPath) = runExceptT $ do
  version <- ExceptT $ liftIO $ getGeckoDriverVersion maybeFirefoxPath
  ExceptT $ obtainGeckoDriver toolsDir (DownloadGeckoDriverVersion version)
obtainGeckoDriver _ (UseGeckoDriverAt path) = liftIO (doesFileExist path) >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> return $ Right path

-- * Lower level helpers


downloadSeleniumIfNecessary :: Constraints m => FilePath -> m (Either T.Text FilePath)
downloadSeleniumIfNecessary toolsDir = leftOnException' $ do
  let seleniumPath = [i|#{toolsDir}/selenium-server.jar|]
  liftIO (doesFileExist seleniumPath) >>= flip unless (downloadSelenium seleniumPath)
  return seleniumPath
  where
    downloadSelenium :: Constraints m => FilePath -> m ()
    downloadSelenium seleniumPath = void $ do
      info [i|Downloading selenium-server.jar to #{seleniumPath}|]
      curlDownloadToPath defaultSeleniumJarUrl seleniumPath

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
getChromeDriverPath toolsDir (ChromeDriverVersionTuple (w, x, y, z)) = [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{chromeDriverExecutable}|]
getChromeDriverPath toolsDir (ChromeDriverVersionExactUrl (w, x, y, z) _) = [i|#{toolsDir}/chromedrivers/#{w}.#{x}.#{y}.#{z}/#{chromeDriverExecutable}|]

getGeckoDriverPath :: FilePath -> GeckoDriverVersion -> FilePath
getGeckoDriverPath toolsDir (GeckoDriverVersion (x, y, z)) = [i|#{toolsDir}/geckodrivers/#{x}.#{y}.#{z}/#{geckoDriverExecutable}|]

chromeDriverExecutable :: T.Text
chromeDriverExecutable = case detectPlatform of
  Windows -> "chromedriver.exe"
  _ -> "chromedriver"

geckoDriverExecutable :: T.Text
geckoDriverExecutable = case detectPlatform of
  Windows -> "geckodriver.exe"
  _ -> "geckodriver"

downloadAndUnzipToPath :: (MonadUnliftIO m, MonadLogger m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  withSystemTempDirectory "sandwich-webdriver-tool-download" $ \dir -> do
    curlDownloadToPath (T.unpack downloadPath) (dir </> "temp.zip")

    createProcessWithLogging ((proc "unzip" ["temp.zip", "-d", "unzipped"]) { cwd = Just dir })
      >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)
    let unzipped = dir </> "unzipped"

    executables <- (filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcessWithLogging (proc "find" [unzipped, "-executable", "-type", "f"]) ""
    case executables of
      [] -> liftIO $ throwIO $ userError [i|No executable found in file downloaded from #{downloadPath}|]
      [x] -> do
        liftIO $ copyFile (T.unpack x) localPath
        createProcessWithLogging (shell [i|chmod u+x #{localPath}|])
          >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)
      xs -> liftIO $ throwIO $ userError [i|Found multiple executable found in file downloaded from #{downloadPath}: #{xs}|]

downloadAndUntarballToPath :: (MonadUnliftIO m, MonadLogger m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUntarballToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  createProcessWithLogging (shell [i|wget -qO- #{downloadPath} | tar xvz  -C #{takeDirectory localPath}|])
    >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)
  createProcessWithLogging (shell [i|chmod u+x #{localPath}|])
    >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)

curlDownloadToPath :: (MonadUnliftIO m, MonadLogger m) => String -> FilePath -> m ()
curlDownloadToPath downloadPath localPath = do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  p <- createProcessWithLogging (proc "curl" [downloadPath, "-o", localPath, "-s"])
  liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (\t -> unless t s)
