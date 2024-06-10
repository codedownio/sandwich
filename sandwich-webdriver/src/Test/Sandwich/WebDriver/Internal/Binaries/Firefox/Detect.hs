{-# LANGUAGE CPP #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Detect (
  detectFirefoxVersion
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
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Safe
import System.Exit
import System.Process
import Test.Sandwich.WebDriver.Internal.Binaries.DetectPlatform
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif


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

tReadMay :: T.Text -> Maybe Int
tReadMay = readMay . T.unpack
