{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Sandwich.WebDriver.Internal.Capabilities.Extra (
  configureHeadlessChromeCapabilities
  , configureHeadlessFirefoxCapabilities
  , configureChromeDownloadCapabilities
  , configureFirefoxDownloadCapabilities

  , configureChromeUserDataDir
  , configureChromeNoSandbox
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.Aeson as A
import Data.Function
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Lens.Micro
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome.Detect (detectChromeVersion)
import Test.Sandwich.WebDriver.Internal.Binaries.Chrome.Types (ChromeVersion(..))
import Test.Sandwich.WebDriver.Internal.Types
import Test.WebDriver.Capabilities as W
import Test.WebDriver.Profile


#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as HM
#else
import qualified Data.HashMap.Strict        as HM
#endif


type Constraints m = (HasCallStack, MonadLogger m, MonadUnliftIO m, MonadMask m)

-- | Add headless configuration to the Chrome browser
configureHeadlessChromeCapabilities :: (Constraints m) => WdOptions -> RunMode -> W.Capabilities -> m W.Capabilities
configureHeadlessChromeCapabilities _wdOptions (RunHeadless (HeadlessConfig {..})) caps@(W.Capabilities {_capabilitiesGoogChromeOptions=(Just chromeOptions)}) = do
  chromeBinaryPath <- case W._chromeOptionsBinary chromeOptions of
    Nothing -> expectationFailure [i|Chrome capabilities didn't define chromeBinary in configureHeadlessCapabilities|]
    Just x -> pure x

  headlessArg <- liftIO (detectChromeVersion chromeBinaryPath) >>= \case
    Left err -> do
      warn [i|Couldn't determine chrome version when configuring headless capabilities (err: #{err}); passing --headless|]
      return "--headless"
    Right (ChromeVersion (major, _, _, _))
      -- See https://www.selenium.dev/blog/2023/headless-is-going-away/
      | major >= 110 -> return "--headless=new"
      | otherwise -> return "--headless"

  let finalChromeOptions = chromeOptions
                         & over chromeOptionsArgs (Just . (\x -> headlessArg:resolution:x) . fromMaybe [])

  return (caps { W._capabilitiesGoogChromeOptions = Just finalChromeOptions })

  where
    resolution = [i|--window-size=#{w},#{h}|]
    (w, h) = fromMaybe (1920, 1080) headlessResolution
configureHeadlessChromeCapabilities _ _ browser = return browser

-- | Add headless configuration to the Firefox capabilities
configureHeadlessFirefoxCapabilities :: (Constraints m) => WdOptions -> RunMode -> W.Capabilities -> m W.Capabilities
configureHeadlessFirefoxCapabilities _ (RunHeadless (HeadlessConfig {})) caps@(W.Capabilities {_capabilitiesMozFirefoxOptions=(Just firefoxOptions)}) =
  return (caps { W._capabilitiesMozFirefoxOptions = Just finalFirefoxOptions })
  where
    finalFirefoxOptions = firefoxOptions
                        & over firefoxOptionsArgs (Just . (\x -> headlessArg:x) . fromMaybe [])

    headlessArg = "-headless"
configureHeadlessFirefoxCapabilities _ _ browser = return browser


-- | Configure download capabilities to set the download directory and disable prompts
-- (since you can't test download prompts using Selenium)
configureChromeDownloadCapabilities :: Monad m => String -> Capabilities -> m Capabilities
configureChromeDownloadCapabilities downloadDir caps@(W.Capabilities {_capabilitiesGoogChromeOptions=(Just chromeOptions)}) =
  return $ caps { W._capabilitiesGoogChromeOptions=(Just finalChromeOptions) }
  where
    finalChromeOptions = chromeOptions
                       & set chromeOptionsPrefs (Just prefs)

    prefs :: A.Object
    prefs = basePrefs
          & foldl (.) id [HM.insert k v | (k, v) <- downloadPrefs]

    basePrefs :: A.Object
    basePrefs = case HM.lookup "prefs" (fromMaybe mempty (W._chromeOptionsPrefs chromeOptions)) of
      Just (A.Object hm) -> hm
      Just x -> error [i|Expected chrome prefs to be object, got '#{x}'.|]
      Nothing -> mempty

    downloadPrefs = [
      ("profile.default_content_setting_values.automatic_downloads", A.Number 1)
      , ("profile.content_settings.exceptions.automatic_downloads.*.setting", A.Number 1)
      , ("download.prompt_for_download", A.Bool False)
      , ("download.directory_upgrade", A.Bool True)
      , ("download.default_directory", A.String (T.pack downloadDir))
      ]
configureChromeDownloadCapabilities _ browser = return browser

-- | Configure download capabilities to set the download directory and disable prompts
-- (since you can't test download prompts using Selenium)
configureFirefoxDownloadCapabilities :: (
  MonadIO m
  ) => [Char] -> W.Capabilities -> m W.Capabilities
configureFirefoxDownloadCapabilities downloadDir caps@(W.Capabilities {_capabilitiesMozFirefoxOptions=(Just firefoxOptions)}) = do
  profile <- case W._firefoxOptionsProfile firefoxOptions of
    Just x -> pure x
    Nothing -> liftIO $ defaultFirefoxProfile
      & addPref "browser.download.folderList" (2 :: Int)
      & addPref "browser.download.manager.showWhenStarting" False
      & addPref "browser.download.dir" downloadDir
      & addPref "browser.helperApps.neverAsk.saveToDisk" ("*" :: String)
      & prepareFirefoxProfile

  let finalFirefoxOptions = firefoxOptions
                          & set firefoxOptionsProfile (Just profile)

  return (caps { W._capabilitiesMozFirefoxOptions = Just finalFirefoxOptions  })
configureFirefoxDownloadCapabilities _ browser = return browser

-- | Pass the @--user-data-dir@ argument to Chrome, putting the directory inside
-- the test tree. This is usually better than allowing tests to leave stuff in
-- /tmp etc.
--
-- Note that chromedriver sometimes reports an error like the following:
-- "session not created: probably user data directory is already in use, please
-- specify a unique value for --user-data-dir argument, or don't use
-- --user-data-dir".
--
-- This is usually a red herring, chromedriver seems to report it whenever the
-- browser fails to start up for whatever reason.
configureChromeUserDataDir :: (Constraints m, HasBaseContextMonad context m, MonadFail m) => W.Capabilities -> m W.Capabilities
configureChromeUserDataDir caps@(W.Capabilities {_capabilitiesGoogChromeOptions=(Just chromeOptions)}) = do
  Just dir <- getCurrentFolder
  userDataDir <- liftIO $ createTempDirectory dir "chrome-user-data-dir"
  let arg = [i|--user-data-dir=#{userDataDir}|]
  let finalChromeOptions = chromeOptions
                         & over chromeOptionsArgs (Just . (arg :) . fromMaybe [])
  return (caps { W._capabilitiesGoogChromeOptions = Just finalChromeOptions })
configureChromeUserDataDir caps = return caps


-- | This is to make it possible to use Chrome installed by Nix, avoiding errors like this:
--
-- [76593:76593:0608/101002.800744:FATAL:setuid_sandbox_host.cc(163)] The SUID sandbox helper binary was found,
-- but is not configured correctly. Rather than run without sandboxing I'm aborting now. You need to make sure
-- that /nix/store/6sshf2mnzfy72sqr7k9f2mi36ccczr9a-google-chrome-130.0.6723.91/share/google/chrome/chrome-sandbox
-- is owned by root and has mode 4755.
configureChromeNoSandbox :: (Constraints m) => WdOptions -> W.Capabilities -> m W.Capabilities
configureChromeNoSandbox (WdOptions {chromeNoSandbox=True}) caps@(W.Capabilities {_capabilitiesGoogChromeOptions=(Just chromeOptions)}) = do
  let arg = "--no-sandbox"
  let finalChromeOptions = chromeOptions
                         & over chromeOptionsArgs (Just . (arg :) . fromMaybe [])
  return (caps { W._capabilitiesGoogChromeOptions = Just finalChromeOptions })
configureChromeNoSandbox _ caps = return caps
