{-# LANGUAGE CPP #-}

module Test.Sandwich.WebDriver.Internal.Binaries.DetectChrome (
  detectChromeVersion
  , getChromeDriverVersion
  , getChromeDriverDownloadUrl
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as LB
import Data.Function
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP.Client
import Network.HTTP.Conduit (simpleHttp)
import Safe
import System.Directory (findExecutable)
import System.Exit
import System.Process
import Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util


findChromeInEnvironment :: IO String
findChromeInEnvironment =
  flip fix candidates $ \loop cs -> case cs of
    [] -> pure "google-chrome" -- Give up
    (candidate:rest) -> findExecutable candidate >>= \case
      Nothing -> loop rest
      Just _ -> pure candidate
  where
    candidates = [
      "google-chrome"
      , "google-chrome-stable" -- May be found on NixOS
      ]

detectChromeVersion :: Maybe FilePath -> IO (Either T.Text ChromeVersion)
detectChromeVersion maybeChromePath = leftOnException $ runExceptT $ do
  chromeToUse <- liftIO $ maybe findChromeInEnvironment pure maybeChromePath

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
getChromeDriverVersion' (ChromeVersion (w, x, y, _))
  | w < 115 = do
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
  | otherwise = do
      let url = [i|https://googlechromelabs.github.io/chrome-for-testing/known-good-versions-with-downloads.json|]
      handle (\(e :: HttpException) -> do
                return $ Left [i|Error when requesting '#{url}': '#{e}'|]
             )
             (do
                 result :: LB.ByteString <- simpleHttp url
                 undefined
             )

getChromeDriverDownloadUrl :: ChromeDriverVersion -> Platform -> T.Text
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) Linux = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_linux64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) OSX = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_mac64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersion (w, x, y, z)) Windows = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_win32.zip|]

-- * Util

tReadMay = readMay . T.unpack
