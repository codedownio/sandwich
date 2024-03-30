{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.WebDriver (
  -- * Introducing a WebDriver server
  introduceWebDriver
  , introduceWebDriverViaNix
  , introduceWebDriverOptions
  , addCommandLineOptionsToWdOptions

  -- * Running an example in a given session
  , withSession
  , withSession1
  , withSession2

  -- * Managing sessions
  , getSessions
  , closeCurrentSession
  , closeSession
  , closeAllSessions
  , closeAllSessionsExcept
  , Session

  -- * Lower-level allocation functions
  , allocateWebDriver
  , allocateWebDriver'
  , cleanupWebDriver
  , cleanupWebDriver'

  -- * Re-exports
  , module Test.Sandwich.WebDriver.Class
  , module Test.Sandwich.WebDriver.Config
  , module Test.Sandwich.WebDriver.Types
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Internal
import Test.Sandwich.WebDriver.Class
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Internal.Action
import Test.Sandwich.WebDriver.Internal.StartWebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W
import UnliftIO.MVar


-- | This is the main 'introduce' method for creating a WebDriver.
introduceWebDriver :: (
  BaseMonadContext m context
  ) => WdOptions -> SpecFree (LabelValue "webdriver" WebDriver :> context) m () -> SpecFree context m ()
introduceWebDriver wdOptions = undefined -- introduce "Introduce WebDriver session" webdriver (allocateWebDriver wdOptions) cleanupWebDriver

introduceWebDriverViaNix :: (
  BaseMonadContext m context, HasNixContext context, HasFile context "java"
  ) => WdOptions -> SpecFree (LabelValue "webdriver" WebDriver :> context) m () -> SpecFree context m ()
introduceWebDriverViaNix wdOptions =
  introduce "Introduce WebDriver session" webdriver (allocateWebDriver wdOptions) cleanupWebDriver

-- | Same as introduceWebDriver, but merges command line options into the 'WdOptions'.
introduceWebDriverOptions :: forall a context m. (
  BaseMonadContext m context, HasCommandLineOptions context a, HasFile context "java"
  )
  => WdOptions -> SpecFree (LabelValue "webdriver" WebDriver :> context) m () -> SpecFree context m ()
introduceWebDriverOptions wdOptions = introduce "Introduce WebDriver session" webdriver alloc cleanupWebDriver
  where alloc = do
          clo <- getCommandLineOptions
          allocateWebDriver (addCommandLineOptionsToWdOptions @a clo wdOptions)

-- | Allocate a WebDriver using the given options.
allocateWebDriver :: (BaseMonad m, HasBaseContext context, HasFile context "java") => WdOptions -> ExampleT context m WebDriver
allocateWebDriver wdOptions = do
  debug "Beginning allocateWebDriver"
  dir <- fromMaybe "/tmp" <$> getCurrentFolder
  startWebDriver wdOptions dir

-- | Allocate a WebDriver using the given options and putting logs under the given path.
allocateWebDriver' :: FilePath -> WdOptions -> IO WebDriver
allocateWebDriver' runRoot wdOptions = do
  runNoLoggingT $ flip runReaderT (undefined :: LabelValue "file-java" (EnvironmentFile "java")) $ startWebDriver wdOptions runRoot

-- | Clean up the given WebDriver.
cleanupWebDriver :: (BaseMonad m) => WebDriver -> ExampleT context m ()
cleanupWebDriver sess = do
  closeAllSessions sess
  stopWebDriver sess

-- | Clean up the given WebDriver without logging.
cleanupWebDriver' :: WebDriver -> IO ()
cleanupWebDriver' sess = do
  runNoLoggingT $ do
    closeAllSessions sess
    stopWebDriver sess

-- | Run a given example using a given Selenium session.
withSession :: forall m context a. (WebDriverMonad m context) => Session -> ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession session (ExampleT readerMonad) = do
  WebDriver {..} <- getContext webdriver
  -- Create new session if necessary (this can throw an exception)
  sess <- modifyMVar wdSessionMap $ \sessionMap -> case M.lookup session sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      debug [i|Creating session '#{session}'|]
      sess'' <- liftIO $ W.mkSession wdConfig
      let sess' = sess'' { W.wdSessHistUpdate = W.unlimitedHistory }
      sess <- liftIO $ W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
      return (M.insert session sess sessionMap, sess)

  ref <- liftIO $ newIORef sess

  -- Not used for now, but previous libraries have use a finally to grab the final session on exception.
  -- We could do the same here, but it's not clear that it's needed.
  let f :: m a -> m a = id

  ExampleT (withReaderT (\ctx -> LabelValue (session, ref) :> ctx) $ mapReaderT (mapLoggingT f) readerMonad)

-- | Convenience function. 'withSession1' = 'withSession' "session1"
withSession1 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession1 = withSession "session1"

-- | Convenience function. 'withSession2' = 'withSession' "session2"
withSession2 :: WebDriverMonad m context => ExampleT (ContextWithSession context) m a -> ExampleT context m a
withSession2 = withSession "session2"

-- | Get all existing session names
getSessions :: (MonadReader context m, WebDriverMonad m context) => m [Session]
getSessions = do
  WebDriver {..} <- getContext webdriver
  M.keys <$> liftIO (readMVar wdSessionMap)

-- | Merge the options from the 'CommandLineOptions' into some 'WdOptions'.
addCommandLineOptionsToWdOptions :: CommandLineOptions a -> WdOptions -> WdOptions
addCommandLineOptionsToWdOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})}) wdOptions@(WdOptions {..}) = wdOptions {
  capabilities = case optFirefox of
    Just UseFirefox -> firefoxCapabilities fbp
    Just UseChrome -> chromeCapabilities cbp
    Nothing -> case cbp of
      Just p -> chromeCapabilities (Just p)
      Nothing -> case fbp of
        Just p -> firefoxCapabilities (Just p)
        Nothing -> capabilities

  , runMode = case optDisplay of
      Nothing -> runMode
      Just Headless -> RunHeadless defaultHeadlessConfig
      Just Xvfb -> RunInXvfb (defaultXvfbConfig { xvfbStartFluxbox = optFluxbox })
      Just Current -> Normal

  , seleniumToUse = maybe seleniumToUse UseSeleniumAt optSeleniumJar

  , chromeBinaryPath = cbp
  , chromeDriverToUse = maybe chromeDriverToUse UseChromeDriverAt optChromeDriver

  , firefoxBinaryPath = fbp
  , geckoDriverToUse = maybe geckoDriverToUse UseGeckoDriverAt optGeckoDriver
  }

  where
    cbp = optChromeBinary <|> chromeBinaryPath
    fbp = optFirefoxBinary <|> firefoxBinaryPath
