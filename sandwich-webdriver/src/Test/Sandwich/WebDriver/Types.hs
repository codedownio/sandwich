{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
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

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import GHC.Stack
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types.Status as N
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Dependencies
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Types as W
import UnliftIO.Exception as ES

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
#else
import Data.Hashable
import qualified Data.HashMap.Strict        as HM
#endif


#if MIN_VERSION_aeson(2,0,0)
aesonLookup :: Text -> HM.KeyMap v -> Maybe v
aesonLookup = HM.lookup . A.fromText
#else
aesonLookup :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
aesonLookup = HM.lookup
#endif

instance (MonadIO m, HasWebDriverSessionContext context) => W.SessionState (ExampleT context m) where
  getSession = do
    (_, sess) <- getContext webdriverSession
    return sess

-- This implementation of 'W.WebDriverBase' provides logging for the requests/responses.
instance (MonadUnliftIO m, HasTestWebDriverContext context) => W.WebDriverBase (ExampleT context m) where
  doCommandBase driver method path args = do
    truncationLength <- (webDriverResponseMaxLogLength . wdOptions) <$> getContext webdriver

    let req = W.mkDriverRequest driver method path args

    when (truncationLength /= Just 0) $
      debug [i|--> #{HC.method req} #{HC.path req}#{HC.queryString req} (#{truncateForLogging truncationLength (showRequestBody (HC.requestBody req))})|]

    response <- tryAny (liftIO $ HC.httpLbs req (W._driverManager driver)) >>= either throwIO return
    let (N.Status code _) = HC.responseStatus response

    when (truncationLength /= Just 0) $
      if | code >= 200 && code < 300 -> case A.eitherDecode (HC.responseBody response) of
             -- For successful responses, try to pull out the "value" and show it
             Right (A.Object (aesonLookup "value" -> Just value)) -> debug [i|<-- #{code} #{truncateForLogging truncationLength (A.encode value)}|]
             _ -> debug [i|<-- #{code} #{truncateForLogging truncationLength (HC.responseBody response)}|]
         -- For non-successful responses, log the entire response.
         -- Reading the WebDriver spec, it would probably be sufficient to just show the "value" as above,
         -- plus the HTTP status message.
         | otherwise -> debug [i|<-- #{code} #{truncateTextForLogging truncationLength (T.pack (show response))}|]

    return response

    where
      showRequestBody :: HC.RequestBody -> BL.ByteString
      showRequestBody (HC.RequestBodyLBS bytes) = bytes
      showRequestBody (HC.RequestBodyBS bytes) = BL.fromStrict bytes
      showRequestBody _ = "<request body>"

      -- Measure and slice on the raw bytes (both O(1)) and only decode the retained prefix,
      -- so large bodies (e.g. base64 screenshots) aren't decoded in full.
      truncateForLogging :: Maybe Int -> BL.ByteString -> Text
      truncateForLogging maxLen bytes = case maxLen of
        Just n | len > fromIntegral n ->
          decodeUtf8With lenientDecode (BL.toStrict (BL.take (fromIntegral n) bytes))
            <> [i| ... (#{len - fromIntegral n} more bytes)|]
        _ -> decodeUtf8With lenientDecode (BL.toStrict bytes)
        where
          len = BL.length bytes

      -- Like 'truncateForLogging', but for a value that's already 'Text' (truncates by character).
      truncateTextForLogging :: Maybe Int -> Text -> Text
      truncateTextForLogging maxLen t = case maxLen of
        Just n | T.length t > n -> T.take n t <> [i| ... (#{T.length t - n} more characters)|]
        _ -> t

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
