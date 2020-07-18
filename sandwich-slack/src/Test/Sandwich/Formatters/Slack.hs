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
import Data.Function
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time
import Test.Sandwich
import Test.Sandwich.Internal
import Test.Sandwich.Internal.Formatters
import Web.Slack.ProgressBar

data SlackFormatter = SlackFormatter {
  slackFormatterSlackConfig :: SlackConfig
  -- ^ Slack credentials
  , slackFormatterTopMessage :: Maybe String
  -- ^ Message to put above the progress bar. For example, the name of the test suite and a link to the run in the CI system.
  , slackFormatterChannel :: String
  -- ^ Slack channel on which to create the progress bar.
  }

instance Formatter SlackFormatter where
  runFormatter = runApp

runApp :: SlackFormatter -> [RunNode BaseContext] -> BaseContext -> IO ()
runApp pf@(SlackFormatter {..}) rts bc = do
  startTime <- getCurrentTime

  rtsFixed <- atomically $ mapM fixRunTree rts
  let pbi = publishTree slackFormatterTopMessage 0 rtsFixed
  pb <- (createProgressBar slackFormatterSlackConfig (T.pack slackFormatterChannel) pbi) >>= \case
    Left err -> throwIO $ userError $ T.unpack err
    Right pb -> return pb

  currentFixedTree <- newTVarIO rtsFixed
  fix $ \loop -> do
    newFixedTree <- atomically $ do
      currentFixed <- readTVar currentFixedTree
      newFixed <- mapM fixRunTree rts
      when (fmap getCommons newFixed == fmap getCommons currentFixed) retry
      writeTVar currentFixedTree newFixed
      return newFixed

    now <- getCurrentTime
    let pbi' = publishTree slackFormatterTopMessage (diffUTCTime now startTime) newFixedTree
    tryAny (updateProgressBar slackFormatterSlackConfig pb pbi') >>= \case
      Left err -> return () -- TODO: notify somehow?
      Right (Left err) -> return ()
      Right (Right ()) -> return ()

    if | all (isDone . runTreeStatus . runNodeCommon) newFixedTree -> return ()
       | otherwise -> do
           threadDelay 100000 -- Sleep 100ms
           loop


publishTree topMessage elapsed tree = pbi
  where
    pbi = ProgressBarInfo { progressBarInfoTopMessage = T.pack <$> topMessage
                          , progressBarInfoBottomMessage = Just bottomMessage
                          , progressBarInfoSize = Just (100.0 * (fromIntegral (succeeded + pending + failed) / (fromIntegral total)))
                          , progressBarInfoAttachments = Nothing
                          }

    bottomMessage = T.intercalate "\n" $ catMaybes [
      maybeMessage
      , Just [i|#{succeeded} succeeded, #{failed} failed, #{pending} pending of #{total} (#{formatNominalDiffTime elapsed}s elapsed)|]
      ]

    maybeMessage = Nothing

    total = countWhere isItBlock tree
    succeeded = countWhere isSuccessItBlock tree
    pending = countWhere isPendingItBlock tree
    failed = countWhere isFailedItBlock tree
    totalRunningTests = countWhere isRunningItBlock tree
    totalNotStartedTests = countWhere isNotStartedItBlock tree

isDone :: Status -> Bool
isDone (Done {}) = True
isDone _ = False
