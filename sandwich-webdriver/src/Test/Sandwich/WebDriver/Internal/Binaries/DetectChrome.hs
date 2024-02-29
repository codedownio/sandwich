{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Sandwich.WebDriver.Internal.Binaries.DetectChrome (
  detectChromeVersion
  , getChromeDriverVersion
  , getChromeDriverDownloadUrl
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson as A
import qualified Data.ByteString.Lazy as LB
import Data.Function
import Data.Map as M hiding (mapMaybe)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Conduit (simpleHttp)
import Safe
import System.Directory (findExecutable)
import System.Exit
import qualified System.Info as SI
import System.Process
import Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util


data PlatformAndUrl = PlatformAndUrl {
  platform :: Text
  , url :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data Version = Version {
  version :: Text
  , revision :: Text
  , downloads :: Map Text [PlatformAndUrl]
  } deriving (Show, Generic, FromJSON, ToJSON)

data JsonResponse = JsonResponse {
  timestamp :: Text
  , versions :: [Version]
  } deriving (Show, Generic, FromJSON, ToJSON)

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
getChromeDriverVersion' (ChromeVersion (w, x, y, z))
  | w < 115 = do
      let url = [i|https://chromedriver.storage.googleapis.com/LATEST_RELEASE_#{w}.#{x}.#{y}|]
      handle (\(e :: HttpException) -> do
                return $ Left [i|Error when requesting '#{url}': '#{e}'|]
             )
             (do
                 result :: T.Text <- (TL.toStrict . TL.decodeUtf8) <$> simpleHttp url
                 case T.splitOn "." result of
                   [tReadMay -> Just w', tReadMay -> Just x', tReadMay -> Just y', tReadMay -> Just z'] -> return $ Right $ ChromeDriverVersionTuple (w', x', y', z')
                   _ -> return $ Left [i|Failed to parse chromedriver version from string: '#{result}'|]
             )
  | otherwise = do
      let url = [i|https://googlechromelabs.github.io/chrome-for-testing/known-good-versions-with-downloads.json|]
      handle (\(e :: HttpException) -> do
                return $ Left [i|Error when requesting '#{url}': '#{e}'|]
             )
             (do
                 result :: LB.ByteString <- simpleHttp url
                 case A.eitherDecode result of
                   Left err -> return $ Left [i|Failed to decode response from '#{url}': #{err}|]
                   Right (response :: JsonResponse) -> do
                     let matchingVersions = [v | v@(Version {..}) <- versions response
                                               , [i|#{w}.#{x}.#{y}.|] `T.isPrefixOf` version]

                     let exactMatch = headMay [v | v@(Version {..}) <- matchingVersions
                                                 , [i|#{w}.#{x}.#{y}.#{z}|] == version]

                     let versionList :: [Version]
                         versionList = (case exactMatch of Nothing -> id; Just v -> (v :)) matchingVersions

                     case headMay (mapMaybe extractSuitableChromeDriver versionList) of
                       Nothing -> return $ Left [i|Couldn't find chromedriver associated with any Chrome release|]
                       Just (tup, url') -> return $ Right $ ChromeDriverVersionExactUrl tup url'
             )

extractSuitableChromeDriver :: Version -> Maybe ((Int, Int, Int, Int), Text)
extractSuitableChromeDriver (Version { version=(parseTuple -> Just tup), downloads=(M.lookup "chromedriver" -> Just platforms) }) =
  case headMay [url | PlatformAndUrl {platform, url} <- platforms
                    , platform == desiredPlatform] of
    Nothing -> Nothing
    Just url -> Just (tup, url)
  where
    desiredPlatform = case (SI.os, SI.arch) of
      ("windows", "x86_64") -> "win64"
      ("windows", "i386") -> "win32"
      ("mingw32", "x86_64") -> "win64"
      ("mingw32", "i386") -> "win32"

      ("darwin", "x86_64") -> "mac-x64"
      ("darwin", "arm") -> "mac-arm64"

      ("linux", _) -> "linux64"
      ("freebsd", _) -> "linux64"
      ("netbsd", _) -> "linux64"
      ("openbsd", _) -> "linux64"

      _ -> "unknown"
extractSuitableChromeDriver _ = Nothing

parseTuple :: Text -> Maybe (Int, Int, Int, Int)
parseTuple (T.splitOn "." -> [tReadMay -> Just w, tReadMay -> Just x, tReadMay -> Just y, tReadMay -> Just z]) = Just (w, x, y, z)
parseTuple _ = Nothing

getChromeDriverDownloadUrl :: ChromeDriverVersion -> Platform -> T.Text
getChromeDriverDownloadUrl (ChromeDriverVersionTuple (w, x, y, z)) Linux = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_linux64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersionTuple (w, x, y, z)) OSX = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_mac64.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersionTuple (w, x, y, z)) Windows = [i|https://chromedriver.storage.googleapis.com/#{w}.#{x}.#{y}.#{z}/chromedriver_win32.zip|]
getChromeDriverDownloadUrl (ChromeDriverVersionExactUrl _ url) _ = url

-- * Util

tReadMay :: T.Text -> Maybe Int
tReadMay = readMay . T.unpack
