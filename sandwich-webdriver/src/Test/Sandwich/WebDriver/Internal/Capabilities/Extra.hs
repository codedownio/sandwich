{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Sandwich.WebDriver.Internal.Capabilities.Extra (
  configureHeadlessCapabilities
  , configureDownloadCapabilities
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Stack
import Lens.Micro
import Lens.Micro.Aeson
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.DetectChrome (detectChromeVersion)
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Firefox.Profile as FF
import UnliftIO.Exception


#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
fromText :: T.Text -> A.Key
fromText = A.fromText
#else
import qualified Data.HashMap.Strict        as HM
fromText :: T.Text -> T.Text
fromText = id
#endif


type Constraints m = (HasCallStack, MonadLogger m, MonadUnliftIO m, MonadBaseControl IO m, MonadMask m)

-- | Add headless configuration to the Chrome browser
configureHeadlessCapabilities :: (Constraints m) => WdOptions -> RunMode -> W.Capabilities -> m W.Capabilities
configureHeadlessCapabilities wdOptions (RunHeadless (HeadlessConfig {..})) caps@(W.Capabilities {W.browser=browser@(W.Chrome {..})}) = do
  chromeBinaryPath <- case chromeBinary of
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

  let browser' = browser { W.chromeOptions = headlessArg:resolution:chromeOptions }

  return (caps { W.browser = browser' })

  where
    resolution = [i|--window-size=#{w},#{h}|]
    (w, h) = fromMaybe (1920, 1080) headlessResolution

-- | Add headless configuration to the Firefox capabilities
configureHeadlessCapabilities _ (RunHeadless (HeadlessConfig {})) caps@(W.Capabilities {W.browser=(W.Firefox {}), W.additionalCaps=ac}) = return (caps { W.additionalCaps = additionalCaps })
  where
    additionalCaps = case L.findIndex (\x -> fst x == "moz:firefoxOptions") ac of
      Nothing -> ("moz:firefoxOptions", A.object [("args", A.Array ["-headless"])]) : ac
      Just i' -> let ffOptions' = snd (ac !! i')
                                & ensureKeyExists "args" (A.Array [])
                                & ((key "args" . _Array) %~ addHeadlessArg) in
        L.nubBy (\x y -> fst x == fst y) (("moz:firefoxOptions", ffOptions') : ac)

    ensureKeyExists :: T.Text -> A.Value -> A.Value -> A.Value
    ensureKeyExists key' _ val@(A.Object (HM.lookup (fromText key') -> Just _)) = val
    ensureKeyExists key' defaultVal (A.Object m@(HM.lookup (fromText key') -> Nothing)) = A.Object (HM.insert (fromText key') defaultVal m)
    ensureKeyExists _ _ _ = error "Expected Object in ensureKeyExists"

    addHeadlessArg :: V.Vector A.Value -> V.Vector A.Value
    addHeadlessArg xs | (A.String "-headless") `V.elem` xs = xs
    addHeadlessArg xs = (A.String "-headless") `V.cons` xs

configureHeadlessCapabilities _ (RunHeadless {}) browser = error [i|Headless mode not yet supported for browser '#{browser}'|]
configureHeadlessCapabilities _ _ browser = return browser


-- | Configure download capabilities to set the download directory and disable prompts
-- (since you can't test download prompts using Selenium)
configureDownloadCapabilities :: (
  MonadIO m, MonadBaseControl IO m
  ) => [Char] -> W.Capabilities -> m W.Capabilities
configureDownloadCapabilities downloadDir caps@(W.Capabilities {W.browser=browser@(W.Firefox {..})}) = do
  case ffProfile of
    Nothing -> return ()
    Just _ -> liftIO $ throwIO $ userError [i|Can't support Firefox profile yet.|]

  profile <- FF.defaultProfile
    & FF.addPref "browser.download.folderList" (2 :: Int)
    & FF.addPref "browser.download.manager.showWhenStarting" False
    & FF.addPref "browser.download.dir" downloadDir
    & FF.addPref "browser.helperApps.neverAsk.saveToDisk" ("*" :: String)
    & FF.prepareProfile

  return (caps { W.browser = browser { W.ffProfile = Just profile } })
configureDownloadCapabilities downloadDir caps@(W.Capabilities {W.browser=browser@(W.Chrome {..})}) = return $ caps { W.browser=browser' }
  where
    browser' = browser { W.chromeExperimentalOptions = options }

    basePrefs :: A.Object
    basePrefs = case HM.lookup "prefs" chromeExperimentalOptions of
      Just (A.Object hm) -> hm
      Just x -> error [i|Expected chrome prefs to be object, got '#{x}'.|]
      Nothing -> mempty

    prefs :: A.Object
    prefs = basePrefs
          & foldl (.) id [HM.insert k v | (k, v) <- downloadPrefs]

    options = HM.insert "prefs" (A.Object prefs) chromeExperimentalOptions

    downloadPrefs = [("profile.default_content_setting_values.automatic_downloads", A.Number 1)
                    , ("profile.content_settings.exceptions.automatic_downloads.*.setting", A.Number 1)
                    , ("download.prompt_for_download", A.Bool False)
                    , ("download.directory_upgrade", A.Bool True)
                    , ("download.default_directory", A.String (T.pack downloadDir))]
configureDownloadCapabilities _ browser = return browser
