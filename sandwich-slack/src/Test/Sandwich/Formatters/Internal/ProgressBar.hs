{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Sandwich.Formatters.Internal.ProgressBar where


import Control.Lens hiding ((??))
import Control.Monad.Except
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Data.Char
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Test.Sandwich.Formatters.Internal.Core
import Test.Sandwich.Formatters.Internal.Types

-- | Create a progress bar message on the given channel.
-- Returns a 'ProgressBar' which can be used to update the message by calling 'updateProgressBar'.
createProgressBar :: SlackConfig -> ChannelName -> ProgressBarInfo -> IO (Either T.Text ProgressBar)
createProgressBar slackConfig channel pbi@(ProgressBarInfo {..}) =
  (runExceptT $ postMessage slackConfig channel message (getAttachments pbi) (addMessageToBlocks message progressBarInfoBlocks)) >>= \case
    Left err -> return $ Left [i|Failed to send initial result: '#{err}'|]
    Right resp -> case (resp ^? key "ts" . _String, resp ^? key "channel" . _String) of
      (Just ts, Just chan) -> return $ Right $ ProgressBar ts chan
      _ -> return $ Left [i|Couldn't find timestamp and/or channel in response|]
  where
    message = getMessage pbi

-- | Update an existing progress bar.
updateProgressBar :: SlackConfig -> ProgressBar -> ProgressBarInfo -> IO (Either T.Text ())
updateProgressBar slackConfig (ProgressBar {..}) pbi@(ProgressBarInfo {..}) =
  (runExceptT $ updateMessage slackConfig progressBarChannel progressBarTs (getMessage pbi) (getAttachments pbi) (addMessageToBlocks message progressBarInfoBlocks)) >>= \case
    Left err -> return $ Left [i|Failed to update progress bar: '#{err}'|]
    Right _ -> return $ Right ()
  where
    message = getMessage pbi

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

barSized :: Double -> T.Text
barSized n = (T.replicate darkBlocks $ T.singleton $ chr 9608)
             <> (T.replicate lightBlocks $ T.singleton $ chr 9617)
             <> [i| #{roundTo 2 n}%|]
  where darkBlocks = round $ n * multiplier
        lightBlocks = round $ (100 - n) * multiplier
        multiplier = 0.5

        roundTo :: (Fractional a, RealFrac a) => Integer -> a -> a
        roundTo places num = (fromInteger $ round $ num * (10^places)) / (10.0^^places)
