{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sandwich.Formatters.Internal.Core where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Network.Wreq as W
import Test.Sandwich.Formatters.Internal.Types

postMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelName -> T.Text -> [A.Value] -> Maybe [A.Value] -> m Value
postMessage conf cid msg as maybeBlocks =
  makeSlackCall conf "chat.postMessage" $
    (W.param "channel"       .~ [cid])
    . (W.param "text"        .~ [msg])
    . (W.param "attachments" .~ [encode' as])
    . (case maybeBlocks of Nothing -> id; Just blocks -> (W.param "blocks" .~ [encode' $ A.Array $ V.fromList blocks]))
    . (W.param "as_user"     .~ ["true"])

updateMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelName -> T.Text -> T.Text -> [A.Value] -> Maybe [A.Value] -> m ()
updateMessage conf cid ts msg as maybeBlocks =
  void $ makeSlackCall conf "chat.update" $
    (W.param "channel"     .~ [cid])
    . (W.param "text"        .~ [msg])
    . (W.param "attachments" .~ [encode' as])
    . (case maybeBlocks of Nothing -> id; Just blocks -> (W.param "blocks" .~ [encode' $ A.Array $ V.fromList blocks]))
    . (W.param "as_user"     .~ ["true"])
    . (W.param "ts"          .~ [ts])

encode' :: A.ToJSON a => a -> T.Text
encode' = T.decodeUtf8 . BL.toStrict . encode

makeSlackCall :: (MonadError T.Text m, MonadIO m) => SlackConfig -> String -> (W.Options -> W.Options) -> m Value
makeSlackCall conf method setArgs = do
  let url = "https://slack.com/api/" ++ method
  let setToken = W.param "token" .~ [slackApiToken conf]
  let opts = W.defaults & setToken & setArgs
  rawResp <- liftIO $ W.getWith opts url
  resp <- rawResp ^? W.responseBody . _Value ?? "Couldn't parse response"
  case resp ^? key "ok" . _Bool of
    Just True -> return resp
    Just False -> throwError $ resp ^. key "error" . _String
    Nothing -> throwError "Couldn't parse key 'ok' from response"

infixl 7 ??
(??) :: MonadError e m => Maybe a -> e -> m a
x ?? e = maybe (throwError e) return x
