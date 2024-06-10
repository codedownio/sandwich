{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}

module Test.Sandwich.WebDriver.Internal.Binaries (
  downloadSeleniumIfNecessary

  , obtainChromeDriver
  , ChromeDriverToUse(..)
  , downloadChromeDriverIfNecessary

  , obtainGeckoDriver
  , GeckoDriverToUse(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import Test.Sandwich.Logging
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome
import Test.Sandwich.WebDriver.Internal.Binaries.Common
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox
import Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Types
import Test.Sandwich.WebDriver.Internal.Util


type Constraints m = (
  HasCallStack
  , MonadLogger m
  , MonadUnliftIO m
  )

-- * Obtaining binaries

defaultSeleniumJarUrl :: String
defaultSeleniumJarUrl = "https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar"

-- TODO: remove curl dependencies here

-- -- | Manually obtain a Selenium server JAR file, according to the 'SeleniumToUse' policy,
-- -- storing it under the provided 'FilePath' if necessary and returning the exact path.
-- obtainSelenium :: (MonadUnliftIO m, MonadLogger m) => FilePath -> SeleniumToUse -> m (Either T.Text FilePath)
-- obtainSelenium toolsDir (DownloadSeleniumFrom url) = do
--   let path = [i|#{toolsDir}/selenium-server-standalone.jar|]
--   unlessM (liftIO $ doesFileExist path) $
--     curlDownloadToPath url path
--   return $ Right path
-- obtainSelenium toolsDir DownloadSeleniumDefault = do
--   let path = [i|#{toolsDir}/selenium-server-standalone-3.141.59.jar|]
--   unlessM (liftIO $ doesFileExist path) $
--     curlDownloadToPath defaultSeleniumJarUrl path
--   return $ Right path
-- obtainSelenium _ (UseSeleniumAt path) = liftIO (doesFileExist path) >>= \case
--   False -> return $ Left [i|Path '#{path}' didn't exist|]
--   True -> return $ Right path


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
