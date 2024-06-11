{-# LANGUAGE ConstraintKinds #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Selenium where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver.Internal.Binaries.Common
import Test.Sandwich.WebDriver.Internal.Binaries.Selenium.Types
import Test.Sandwich.WebDriver.Internal.Util


type Constraints m = (
  HasCallStack
  , MonadLogger m
  , MonadUnliftIO m
  )

-- * Obtaining binaries

defaultSeleniumJarUrl :: String
defaultSeleniumJarUrl = "https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar"

-- | Manually obtain a Selenium server JAR file, according to the 'SeleniumToUse' policy,
-- storing it under the provided 'FilePath' if necessary and returning the exact path.
obtainSelenium :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLogger m, MonadFail m
  ) => SeleniumToUse -> m FilePath
obtainSelenium (DownloadSeleniumFrom toolsDir url) = do
  let path = [i|#{toolsDir}/selenium-server-standalone.jar|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath url path
  return path
obtainSelenium (DownloadSeleniumDefault toolsDir) = do
  let path = [i|#{toolsDir}/selenium-server-standalone-3.141.59.jar|]
  unlessM (liftIO $ doesFileExist path) $
    curlDownloadToPath defaultSeleniumJarUrl path
  return path
obtainSelenium (UseSeleniumAt path) = liftIO (doesFileExist path) >>= \case
  False -> expectationFailure [i|Path '#{path}' didn't exist|]
  True -> return path
obtainSelenium (UseSeleniumFromNixpkgs nixContext) = do
  buildNixSymlinkJoin' nixContext ["selenium-server-standalone"] >>=
    liftIO . findFirstFile (return . (".jar" `L.isSuffixOf`))


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
