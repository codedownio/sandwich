{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Sandwich.WebDriver.Types (
  -- * Type aliases to make signatures shorter.
  BaseMonad
  , ContextWithWebdriverDeps
  , ContextWithBaseDeps
  , WebDriverMonad
  , WebDriverSessionMonad

  -- * Context aliases
  , HasBrowserDependencies
  , HasWebDriverContext
  , HasWebDriverSessionContext

  -- * The Xvfb session
  , XvfbSession(..)
  , getXvfbSession
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson as A
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.String.Interpolate
import GHC.Stack
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Internal as WI
import qualified Test.WebDriver.Session as W
import UnliftIO.Exception as ES


instance (MonadIO m, HasWebDriverSessionContext context) => W.WDSessionState (ExampleT context m) where
  getSession = do
    (_, sessVar) <- getContext webdriverSession
    liftIO $ readIORef sessVar
  putSession sess = do
    (_, sessVar) <- getContext webdriverSession
    liftIO $ writeIORef sessVar sess

-- This implementation of 'W.WebDriver' provides logging for the requests/responses.
instance (
  MonadIO m, MonadCatch m, HasWebDriverSessionContext context
  ) => W.WebDriver (ExampleT context m) where
  doCommand method path args = do
    req <- WI.mkRequest method path args
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
    response <- WI.sendHTTPRequest req >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response
    WI.getJSONResult response >>= \case
      Left e -> do
        warn [i|<-- #{code} Exception: #{e}|]
        throwIO e
      Right result -> do
        debug [i|<-- #{code} #{A.encode result}|]
        return result

    where
      showRequestBody :: HC.RequestBody -> ByteString
      showRequestBody (HC.RequestBodyLBS bytes) = BL.toStrict bytes
      showRequestBody (HC.RequestBodyBS bytes) = bytes
      showRequestBody _ = "<request body>"

type HasWebDriverContext context = HasLabel context "webdriver" WebDriver
type HasWebDriverSessionContext context = HasLabel context "webdriverSession" WebDriverSession

type ContextWithWebdriverDeps context =
  LabelValue "webdriver" WebDriver
  :> ContextWithBaseDeps context

type ContextWithBaseDeps context =
  -- | Browser dependencies
  LabelValue "browserDependencies" BrowserDependencies
  -- | Java
  :> FileValue "java"
  -- | Selenium
  :> FileValue "selenium.jar"
  -- | Base context
  :> context

type BaseMonad m context = (HasCallStack, MonadUnliftIO m, MonadMask m, HasBaseContext context)
type WebDriverMonad m context = (HasCallStack, MonadUnliftIO m, HasWebDriverContext context)
type WebDriverSessionMonad m context = (WebDriverMonad m context, MonadReader context m, HasWebDriverSessionContext context)
