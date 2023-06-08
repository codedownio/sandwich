{-# LANGUAGE CPP #-}

module Test.Sandwich.Formatters.Slack.Internal.Core where

import Control.Lens hiding ((??))
import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Network.Wreq as W
import Test.Sandwich.Formatters.Slack.Internal.Types

postMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelName -> T.Text -> [A.Value] -> Maybe [A.Value] -> m Value
postMessage conf cid msg as maybeBlocks =
  makeSlackCall conf "chat.postMessage" $ A.object $ [
    ("token", A.String $ slackApiToken conf)
    , ("channel", A.String cid)
    , ("text", A.String msg)
    , ("attachments", A.Array $ V.fromList as)
    , ("as_user", A.Bool True)
    ]
    <> (case maybeBlocks of Nothing -> []; Just blocks -> [("blocks", A.Array $ V.fromList blocks)])

updateMessage :: (MonadError T.Text m, MonadIO m) => SlackConfig -> ChannelName -> T.Text -> T.Text -> [A.Value] -> Maybe [A.Value] -> m ()
updateMessage conf cid ts msg as maybeBlocks =
  void $ makeSlackCall conf "chat.update" $ A.object $ [
    ("token", A.String $ slackApiToken conf)
    , ("channel", A.String cid)
    , ("text", A.String msg)
    , ("attachments", A.Array $ V.fromList as)
    , ("as_user", A.Bool True)
    , ("ts", A.String ts)
    ]
    <> (case maybeBlocks of Nothing -> []; Just blocks -> [("blocks", A.Array $ V.fromList blocks)])

encode' :: A.ToJSON a => a -> T.Text
encode' = T.decodeUtf8 . BL.toStrict . encode

makeSlackCall :: (MonadError T.Text m, MonadIO m) => SlackConfig -> String -> A.Value -> m Value
makeSlackCall conf method body = do
  let url = "https://slack.com/api/" ++ method
  let opts = W.defaults & (W.header "Authorization" .~ ["Bearer " <> T.encodeUtf8 (slackApiToken conf)])
  rawResp <- liftIO $ W.postWith opts url (body)
  resp <- rawResp ^? W.responseBody . _Value ?? "Couldn't parse response"
  case resp ^? key "ok" . _Bool of
    Just True -> return resp
    Just False -> throwError $ resp ^. key "error" . _String
    Nothing -> throwError "Couldn't parse key 'ok' from response"

infixl 7 ??
(??) :: MonadError e m => Maybe a -> e -> m a
x ?? e = maybe (throwError e) return x
