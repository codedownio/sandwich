{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Sandwich.Formatters.Internal.ProgressBar where


import Control.Lens hiding ((??))
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Int
import Test.Sandwich.Formatters.Internal.Core
import Test.Sandwich.Formatters.Internal.Types

-- | Create a progress bar message on the given channel.
-- Returns a 'ProgressBar' which can be used to update the message by calling 'updateProgressBar'.
createProgressBar :: SlackConfig -> ChannelName -> Maybe Int64 -> ProgressBarInfo -> IO (Either T.Text ProgressBar)
createProgressBar slackConfig channel maxMessageSize pbi@(ProgressBarInfo {..}) =
  (runExceptT $ postMessage slackConfig channel message (getAttachments pbi) blocks) >>= \case
    Left err -> return $ Left [i|Failed to send initial result: '#{err}'. Blocks were '#{A.encode progressBarInfoBlocks}'.|]
    Right resp -> case (resp ^? key "ts" . _String, resp ^? key "channel" . _String) of
      (Just ts, Just chan) -> return $ Right $ ProgressBar ts chan
      _ -> return $ Left [i|Couldn't find timestamp and/or channel in response|]
  where
    message = getMessage pbi
    blocks = truncateBlocksIfNecessary maxMessageSize <$> addMessageToBlocks message progressBarInfoBlocks

-- | Update an existing progress bar.
updateProgressBar :: SlackConfig -> Maybe Int64 -> ProgressBar -> ProgressBarInfo -> IO (Either T.Text ())
updateProgressBar slackConfig maxMessageSize (ProgressBar {..}) pbi@(ProgressBarInfo {..}) =
  (runExceptT $ updateMessage slackConfig progressBarChannel progressBarTs (getMessage pbi) (getAttachments pbi) blocks) >>= \case
    Left err -> return $ Left [i|Failed to update progress bar: '#{err}'|]
    Right _ -> return $ Right ()
  where
    message = getMessage pbi
    blocks = truncateBlocksIfNecessary maxMessageSize <$> addMessageToBlocks message progressBarInfoBlocks

-- * Internal

getMessage :: ProgressBarInfo -> T.Text
getMessage (ProgressBarInfo {..}) =
  T.intercalate "\n" $ catMaybes [progressBarInfoTopMessage
                                 , barSized <$> progressBarInfoSize
                                 , progressBarInfoBottomMessage]

getAttachments :: ProgressBarInfo -> [A.Value]
getAttachments (ProgressBarInfo {..}) = maybe [] (fmap A.toJSON) progressBarInfoAttachments

addMessageToBlocks :: T.Text -> Maybe [A.Value] -> Maybe [A.Value]
addMessageToBlocks _ Nothing = Nothing
addMessageToBlocks msg (Just blocks) = Just (textBlock : blocks)
  where
    textBlock = A.object [
      ("type", A.String "section")
      , ("text", A.object [("type", A.String "mrkdwn")
                          , ("text", A.String msg)])
      ]

-- | This is kind of wasteful, because it requires encoding every block to get the lengths.
-- It's also a little rough because it won't exactly match the length of the encoded blocks list
-- (with brackets etc.) and doesn't take into account the rest of the message. Hopefully it's close
-- enough to correct, but TODO find a way to do the truncating efficiently as part of encoding
-- the message onto the wire.
truncateBlocksIfNecessary :: Maybe Int64 -> [A.Value] -> [A.Value]
truncateBlocksIfNecessary _ [] = []
truncateBlocksIfNecessary Nothing xs = xs
truncateBlocksIfNecessary (Just bytesRemaining) (x:xs) = case BL.length $ A.encode x of
  len | len >= bytesRemaining -> []
  len -> x : (truncateBlocksIfNecessary (Just (bytesRemaining - len - 1)) xs)

barSized :: Double -> T.Text
barSized n = (T.replicate darkBlocks $ T.singleton $ chr 9608)
             <> (T.replicate lightBlocks $ T.singleton $ chr 9617)
             <> [i| #{roundTo 2 n}%|]
  where darkBlocks = round $ n * multiplier
        lightBlocks = round $ (100 - n) * multiplier
        multiplier = 0.5

        roundTo :: (Fractional a, RealFrac a) => Integer -> a -> a
        roundTo places num = (fromInteger $ round $ num * (10^places)) / (10.0^^places)
