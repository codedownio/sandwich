{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Util (
  detectPlatform
  , detectChromeVersion
  , getChromeDriverVersion
  , getChromeDriverDownloadUrl
  , Platform(..)

  , detectFirefoxVersion
  , getGeckoDriverVersion
  , getGeckoDriverDownloadUrl
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit (simpleHttp)
import Safe
import System.Exit
import qualified System.Info as SI
import System.Process
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif


data Platform = Linux | OSX | Windows deriving (Show, Eq)

detectPlatform :: Platform
detectPlatform =  case SI.os of
  "windows" -> Windows
  "linux" -> Linux
  "darwin" -> OSX
  _ -> error [i|Couldn't determine host platform from string: '#{SI.os}'|]

-- * Chrome

detectChromeVersion :: Maybe FilePath -> IO (Either T.Text ChromeVersion)
detectChromeVersion maybeChromePath = leftOnException $ runExceptT $ do
  let chromeToUse = fromMaybe "google-chrome" maybeChromePath
  (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell (chromeToUse <> " --version | grep -Eo \"[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\"")) ""

  rawString <- case exitCode of
                 ExitFailure _ -> throwE [i|Couldn't parse google-chrome version. Stdout: '#{stdout}'. Stderr: '#{stderr}'|]
                 ExitSuccess -> return $ T.strip $ T.pack stdout

  case T.splitOn "." rawString of
    [tReadMay -> Just w, tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z] -> return $ ChromeVersion (w, x, y, z)
    _ -> throwE [i|Failed to parse google-chrome version from string: '#{rawString}'|]

getChromeDriverVersion :: Maybe FilePath -> IO (Either T.Text ChromeDriverVersion)
getChromeDriverVersion maybeChromePath = runExceptT $ do
  chromeVersion <- ExceptT $ liftIO $ detectChromeVersion maybeChromePath
  ExceptT $ getChromeDriverVersion' chromeVersion

getChromeDriverVersion' :: ChromeVersion -> IO (Either T.Text ChromeDriverVersion)
getChromeDriverVersion' (ChromeVersion (w, x, y, _)) = do
  let url = [i|https://chromedriver.storage.googleapis.com/LATEST_RELEASE_#{w}.#{x}.#{y}|]
  handle (\(e :: HttpException) -> do
            return $ Left [i|Error when requesting '#{url}': '#{e}'|]
         )
         (do
             result :: T.Text <- (TL.toStrict . TL.decodeUtf8) <$> simpleHttp url
             case T.splitOn "." result of
               [tReadMay -> Just w, tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z] -> return $ Right $ ChromeDriverVersion (w, x, y, z)
               _ -> return $ Left [i|Failed to parse chromedriver version from string: '#{result}'|]
         )

getChromeDriverDownloadUrl :: ChromeDriverVersion -> Platform -> T.Text
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) Linux = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_linux64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) OSX = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_mac64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) Windows = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_win32.zip|]

-- * Firefox

detectFirefoxVersion :: Maybe FilePath -> IO (Either T.Text FirefoxVersion)
detectFirefoxVersion maybeFirefoxPath = leftOnException $ runExceptT $ do
  let firefoxToUse = fromMaybe "firefox" maybeFirefoxPath
  (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell (firefoxToUse <> " --version | grep -Eo \"[0-9]+\\.[0-9]+(\\.[0-9]+)?\"")) ""

  rawString <- case exitCode of
                 ExitFailure _ -> throwE [i|Couldn't parse firefox version. Stdout: '#{stdout}'. Stderr: '#{stderr}'|]
                 ExitSuccess -> return $ T.strip $ T.pack stdout

  case T.splitOn "." rawString of
    [tReadMay -> Just x, tReadMay -> Just y] -> return $ FirefoxVersion (x, y, 0)
    [tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z] -> return $ FirefoxVersion (x, y, z)
    _ -> throwE [i|Failed to parse firefox version from string: '#{rawString}'|]


getGeckoDriverVersion :: Maybe FilePath -> IO (Either T.Text GeckoDriverVersion)
getGeckoDriverVersion _maybeFirefoxPath = runExceptT $ do
  -- firefoxVersion <- ExceptT $ liftIO $ detectFirefoxVersion maybeFirefoxPath

  -- Just get the latest release on GitHub
  let url = [i|https://api.github.com/repos/mozilla/geckodriver/releases/latest|]
  req <- parseRequest url
  manager <- liftIO newTlsManager
  ExceptT $
    handle (\(e :: HttpException) -> return $ Left [i|Error when requesting '#{url}': '#{e}'|])
           (do
               result <- httpLbs (req { requestHeaders = ("User-Agent", "foo") : (requestHeaders req) }) manager
               case A.eitherDecode $ responseBody result of
                 Right (A.Object (HM.lookup "tag_name" -> Just (A.String tag))) -> do
                   let parts = T.splitOn "." $ T.drop 1 tag
                   case parts of
                     [tReadMay -> Just x, tReadMay -> Just y] -> return $ Right $ GeckoDriverVersion (x, y, 0)
                     [tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z] -> return $ Right $ GeckoDriverVersion (x, y, z)
                     _ -> return $ Left [i|Unexpected geckodriver release tag: '#{tag}'|]
                 val -> return $ Left [i|Failed to decode GitHub releases: '#{val}'|]
           )


getGeckoDriverDownloadUrl :: GeckoDriverVersion -> Platform -> T.Text
getGeckoDriverDownloadUrl (GeckoDriverVersion (x, y, z)) Linux = [i|https://github.com/mozilla/geckodriver/releases/download/v#{x}.#{y}.#{z}/geckodriver-v#{x}.#{y}.#{z}-linux64.tar.gz|]
getGeckoDriverDownloadUrl (GeckoDriverVersion (x, y, z)) OSX = [i|https://github.com/mozilla/geckodriver/releases/download/v#{x}.#{y}.#{z}/geckodriver-v#{x}.#{y}.#{z}-macos.tar.gz|]
getGeckoDriverDownloadUrl (GeckoDriverVersion (x, y, z)) Windows = [i|https://github.com/mozilla/geckodriver/releases/download/v#{x}.#{y}.#{z}/geckodriver-v#{x}.#{y}.#{z}-win32.tar.gz|]

-- * Util

tReadMay = readMay . T.unpack
