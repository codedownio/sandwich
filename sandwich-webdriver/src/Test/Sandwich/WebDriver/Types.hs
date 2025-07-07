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
  , HasTestWebDriverContext
  , HasWebDriverSessionContext

  -- * The Xvfb session
  , XvfbSession(..)
  -- , getXvfbSession
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import GHC.Stack
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Types as W
import UnliftIO.Exception as ES


instance (MonadIO m, HasWebDriverSessionContext context) => W.SessionState (ExampleT context m) where
  getSession = do
    (_, sess) <- getContext webdriverSession
    return sess

-- This implementation of 'W.WebDriver' provides logging for the requests/responses.
instance (MonadUnliftIO m) => W.WebDriverBase (ExampleT context m) where
  doCommandBase driver method path args = do
    let req = W.mkDriverRequest driver method path args
    debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{showRequestBody (HC.requestBody req)})|]
    response <- tryAny (liftIO $ HC.httpLbs req (W._driverManager driver)) >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response
    debug [i|<-- #{code} #{response}|]
    return response

    where
      showRequestBody :: HC.RequestBody -> ByteString
      showRequestBody (HC.RequestBodyLBS bytes) = BL.toStrict bytes
      showRequestBody (HC.RequestBodyBS bytes) = bytes
      showRequestBody _ = "<request body>"

type HasTestWebDriverContext context = HasLabel context "webdriver" TestWebDriverContext
type HasWebDriverSessionContext context = HasLabel context "webdriverSession" WebDriverSession

type ContextWithWebdriverDeps context =
  LabelValue "webdriver" TestWebDriverContext
  :> ContextWithBaseDeps context

type ContextWithBaseDeps context =
  -- | Browser dependencies
  LabelValue "browserDependencies" BrowserDependencies
  -- | Base context
  :> context

type BaseMonad m context = (HasCallStack, MonadUnliftIO m, MonadMask m, HasBaseContext context)
type WebDriverMonad m context = (HasCallStack, MonadUnliftIO m, HasTestWebDriverContext context)
type WebDriverSessionMonad m context = (WebDriverMonad m context, MonadReader context m, HasWebDriverSessionContext context)
