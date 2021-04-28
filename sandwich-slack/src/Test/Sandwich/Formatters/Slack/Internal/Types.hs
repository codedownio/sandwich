{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Sandwich.Formatters.Slack.Internal.Types where

import qualified Data.Aeson as A
import qualified Data.Text as T

-- | Configuration options needed to connect to the Slack API
newtype SlackConfig = SlackConfig {
  slackApiToken :: T.Text
  -- ^ Slack API token
  } deriving (Show)

-- | The state of a progress bar message.
data ProgressBarInfo = ProgressBarInfo {
  progressBarInfoTopMessage :: Maybe T.Text
  -- ^ Message to show above the progress bar
  , progressBarInfoBottomMessage :: Maybe T.Text
  -- ^ Message to show below the progress bar
  , progressBarInfoSize :: Maybe Double
  -- ^ Size of the progress bar, a 'Double' from 0 to 100
  , progressBarInfoAttachments :: Maybe [ProgressBarAttachment]
  -- ^ Slack attachments for the message
  , progressBarInfoBlocks :: Maybe [A.Value]
  -- ^ Structured blocks, using the <https://api.slack.com/block-kit Slack Block Kit>
  }

-- | A Slack attachment.
data ProgressBarAttachment = ProgressBarAttachment {
  progressBarAttachmentText :: T.Text
  -- ^ Attachment text
  , progressBarAttachmentColor :: T.Text
  -- ^ Attachment color
  }
instance A.ToJSON ProgressBarAttachment
  where toJSON (ProgressBarAttachment {..}) = A.object [
          ("text", A.String progressBarAttachmentText)
          , ("color", A.String progressBarAttachmentColor)
          ]

-- | An opaque type representing an existing Slack message.
data ProgressBar = ProgressBar {
  progressBarTs :: T.Text
  , progressBarChannel :: T.Text
  }

data SlackFormatterShowCallStacks =
  SlackFormatterNoCallStacks
  -- ^ Don't include callstacks in failure messages
  | SlackFormatterTopNCallStackFrames Int
  -- ^ Include the top N stack frames
  | SlackFormatterFullCallStack
  -- ^ Include the full callstack

type ChannelName = T.Text
