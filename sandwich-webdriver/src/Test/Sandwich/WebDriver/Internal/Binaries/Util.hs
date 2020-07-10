{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Util (
  detectPlatform
  , detectChromeVersion
  , getChromeDriverVersion
  , getChromeDriverDownloadUrl
  , Platform(..)
  , ChromeVersion(..)
  , ChromeDriverVersion(..)
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Convertible
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Network.HTTP.Conduit
import Safe
import System.Exit
import qualified System.Info as SI
import System.Process
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util

data Platform = Linux | OSX | Windows deriving (Show, Eq)

detectPlatform :: Platform
detectPlatform =  case SI.os of
  "windows" -> Windows
  "linux" -> Linux
  "darwin" -> OSX
  _ -> error [i|Couldn't determine host platform from string: '#{SI.os}'|]

detectChromeVersion :: IO (Either T.Text ChromeVersion)
detectChromeVersion = leftOnException $ runExceptT $ do
  (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell [i|google-chrome --version | grep -Eo "[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+"|]) ""

  rawString <- case exitCode of
                 ExitFailure _ -> throwE [i|Couldn't parse google-chrome version. Stdout: '#{stdout}'. Stderr: '#{stderr}'|]
                 ExitSuccess -> return $ T.strip $ convert stdout

  case T.splitOn "." rawString of
    [tReadMay -> Just w, tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z] -> return $ ChromeVersion (w, x, y, z)
    _ -> throwE [i|Failed to parse google-chrome version from string: '#{rawString}'|]

getChromeDriverVersion :: IO (Either T.Text ChromeDriverVersion)
getChromeDriverVersion = runExceptT $ do
  chromeVersion <- ExceptT $ liftIO detectChromeVersion
  ExceptT $ getChromeDriverVersion' chromeVersion

getChromeDriverVersion' :: ChromeVersion -> IO (Either T.Text ChromeDriverVersion)
getChromeDriverVersion' (ChromeVersion (w, x, y, _)) = do
  let url = [i|https://chromedriver.storage.googleapis.com/LATEST_RELEASE_#{w}.#{x}.#{y}|]
  handle (\(e :: HttpException) -> do
            return $ Left [i|Error when requesting '#{url}': '#{e}'|]
         )
         (do
             result :: T.Text <- convert <$> simpleHttp url
             case T.splitOn "." result of
               [tReadMay -> Just w, tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z] -> return $ Right $ ChromeDriverVersion (w, x, y, z)
               _ -> return $ Left [i|Failed to parse chromedriver version from string: '#{result}'|]
         )

getChromeDriverDownloadUrl :: ChromeDriverVersion -> Platform -> T.Text
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) Linux = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_linux64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) OSX = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_mac64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) Windows = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_win32.zip|]

tReadMay = readMay . convert
