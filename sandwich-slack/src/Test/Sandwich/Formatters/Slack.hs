{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.Slack (
  SlackFormatter(..)
  , SlackConfig(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger hiding (logError)
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time
import Safe
import Test.Sandwich
import Test.Sandwich.Internal
import Test.Sandwich.Internal.Formatters
import Web.Slack.ProgressBar

data SlackFormatter = SlackFormatter {
  slackFormatterSlackConfig :: SlackConfig
  -- ^ Slack credentials
  , slackFormatterTopMessage :: Maybe String
  -- ^ Message to put above the progress bar.
  -- For example, the name of the test suite and a link to the run in the CI system.
  , slackFormatterChannel :: String
  -- ^ Slack channel on which to create the progress bar.
  }

instance Formatter SlackFormatter where
  runFormatter = runApp
  formatterName _ = "slack-formatter"

runApp :: (MonadIO m, MonadCatch m, MonadLogger m) => SlackFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
runApp (SlackFormatter {..}) rts bc = do
  startTime <- liftIO getCurrentTime

  rtsFixed <- liftIO $ atomically $ mapM fixRunTree rts
  let pbi = publishTree slackFormatterTopMessage 0 rtsFixed
  pb <- (liftIO $ createProgressBar slackFormatterSlackConfig (T.pack slackFormatterChannel) pbi) >>= \case
    Left err -> liftIO $ throwIO $ userError $ T.unpack err
    Right pb -> return pb

  currentFixedTree <- liftIO $ newTVarIO rtsFixed
  fix $ \loop -> do
    newFixedTree <- liftIO $ atomically $ do
      currentFixed <- readTVar currentFixedTree
      newFixed <- mapM fixRunTree rts
      when (fmap getCommons newFixed == fmap getCommons currentFixed) retry
      writeTVar currentFixedTree newFixed
      return newFixed

    now <- liftIO getCurrentTime
    let pbi' = publishTree slackFormatterTopMessage (diffUTCTime now startTime) newFixedTree
    tryAny (liftIO $ updateProgressBar slackFormatterSlackConfig pb pbi') >>= \case
      Left err -> logError [i|Error updating progress bar: '#{err}'|]
      Right (Left err) -> logError [i|Inner error updating progress bar: '#{err}'|]
      Right (Right ()) -> return ()

    if | all (isDone . runTreeStatus . runNodeCommon) newFixedTree -> return ()
       | otherwise -> do
           liftIO $ threadDelay 100000 -- Sleep 100ms
           loop


publishTree topMessage elapsed tree = pbi
  where
    pbi = ProgressBarInfo { progressBarInfoTopMessage = T.pack <$> topMessage
                          , progressBarInfoBottomMessage = Just fullBottomMessage
                          , progressBarInfoSize = Just (100.0 * (fromIntegral (succeeded + pending + failed) / (fromIntegral total)))
                          , progressBarInfoAttachments = Just attachments
                          }

    fullBottomMessage = case runningMessage of
      Nothing -> bottomMessage
      Just t -> T.pack t <> "\n" <> bottomMessage

    bottomMessage = T.intercalate "\n" $ catMaybes [
      maybeMessage
      , Just [i|#{succeeded} succeeded, #{failed} failed, #{pending} pending, #{totalRunningTests} running of #{total} (#{formatNominalDiffTime elapsed} elapsed)|]
      ]

    maybeMessage = Nothing

    runningMessage = headMay $ L.sort $ catMaybes $ concatMap (extractValues (\node -> if isRunningItBlock node then Just $ runTreeLabel $ runNodeCommon node else Nothing)) tree

    failures = catMaybes $ concatMap (extractValues (\node -> if isFailedItBlock node then Just $ runTreeLabel $ runNodeCommon node else Nothing)) tree
    attachments = [ProgressBarAttachment (T.pack t) "#ff4136" | t <- failures]

    total = countWhere isItBlock tree
    succeeded = countWhere isSuccessItBlock tree
    pending = countWhere isPendingItBlock tree
    failed = countWhere isFailedItBlock tree
    totalRunningTests = countWhere isRunningItBlock tree
    totalNotStartedTests = countWhere isNotStartedItBlock tree

isDone :: Status -> Bool
isDone (Done {}) = True
isDone _ = False
