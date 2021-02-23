{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- |

module Test.Sandwich.Formatters.Slack (
  SlackFormatter
  , SlackConfig(..)

  , defaultSlackFormatter
  , slackFormatterSlackConfig
  , slackFormatterTopMessage
  , slackFormatterChannel
  , slackFormatterMaxFailures
  , slackFormatterShowFailureReason
  , slackFormatterShowCallStacks
  , slackFormatterVisibilityThreshold
  -- , slackFormatterAttachFailureReport

  , SlackFormatterShowCallStacks(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger hiding (logError)
import qualified Data.Aeson as A
import Data.Foldable
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import Safe
import Test.Sandwich
import Test.Sandwich.Formatters.Internal.Markdown
import Test.Sandwich.Formatters.Internal.ProgressBar
import Test.Sandwich.Formatters.Internal.Types
import Test.Sandwich.Internal
import Test.Sandwich.Internal.Formatters


data SlackFormatter = SlackFormatter {
  slackFormatterSlackConfig :: SlackConfig
  -- ^ Slack credentials
  , slackFormatterTopMessage :: Maybe String
  -- ^ Message to put above the progress bar.
  -- For example, the name of the test suite and a link to the run in the CI system.
  , slackFormatterChannel :: String
  -- ^ Slack channel on which to create the progress bar.
  , slackFormatterMaxFailures :: Maybe Int
  -- ^ Maximum number of failures to include in a message.
  -- If too many are included, it's possible to hit Slack's request limit of 8KB, which
  -- causes the message to fail to update.
  -- Defaults to 30.
  , slackFormatterShowFailureReason :: Bool
  -- ^ Show failure reasons with failures.
  , slackFormatterShowCallStacks :: SlackFormatterShowCallStacks
  -- ^ Show call stacks with failures.
  , slackFormatterVisibilityThreshold :: Maybe Int
  -- ^ If present, filter the headings on failures to only include nodes whose visibility
  -- threshold is less than or equal to the value.
  -- , slackFormatterAttachFailureReport :: Bool
  -- -- ^ After tests are complete, attach a text file containing a complete failure report.
  }

defaultSlackFormatter :: SlackFormatter
defaultSlackFormatter = SlackFormatter {
  slackFormatterSlackConfig = SlackConfig "my-password"
  , slackFormatterTopMessage = Just "Top message"
  , slackFormatterChannel = "slack-channel"
  , slackFormatterMaxFailures = Just 30
  , slackFormatterShowFailureReason = True
  , slackFormatterShowCallStacks = SlackFormatterTopNCallStackFrames 3
  , slackFormatterVisibilityThreshold = Nothing
  -- , slackFormatterAttachFailureReport = False
  }

instance Formatter SlackFormatter where
  formatterName _ = "slack-formatter"
  runFormatter = runApp
  finalizeFormatter _ _ _ = return ()

runApp :: (MonadIO m, MonadCatch m, MonadLogger m) => SlackFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
runApp sf@(SlackFormatter {..}) rts _bc = do
  startTime <- liftIO getCurrentTime

  let extractFromNode node = let RunNodeCommonWithStatus {..} = runNodeCommon node in (runTreeId, (T.pack runTreeLabel, runTreeVisibilityLevel))
  let idToLabelAndVisibilityThreshold = M.fromList $ mconcat [extractValues extractFromNode node | node <- rts]

  rtsFixed <- liftIO $ atomically $ mapM fixRunTree rts
  let pbi = publishTree sf idToLabelAndVisibilityThreshold slackFormatterMaxFailures slackFormatterTopMessage 0 rtsFixed
  pb <- (liftIO $ createProgressBar slackFormatterSlackConfig (T.pack slackFormatterChannel) pbi) >>= \case
    Left err -> liftIO $ throwIO $ userError $ T.unpack err
    Right pb -> return pb

  unless (allIsDone rtsFixed) $ do
    currentFixedTree <- liftIO $ newTVarIO rtsFixed
    fix $ \loop -> do
      newFixedTree <- liftIO $ atomically $ do
        currentFixed <- readTVar currentFixedTree
        newFixed <- mapM fixRunTree rts
        when (fmap getCommons newFixed == fmap getCommons currentFixed) retry
        writeTVar currentFixedTree newFixed
        return newFixed

      now <- liftIO getCurrentTime
      let pbi' = publishTree sf idToLabelAndVisibilityThreshold slackFormatterMaxFailures slackFormatterTopMessage (diffUTCTime now startTime) newFixedTree
      tryAny (liftIO $ updateProgressBar slackFormatterSlackConfig pb pbi') >>= \case
        Left err -> logError [i|Error updating progress bar: '#{err}'|]
        Right (Left err) -> logError [i|Inner error updating progress bar: '#{err}'|]
        Right (Right ()) -> return ()

      if | allIsDone newFixedTree -> do
             debug [i|All tree nodes are done, exiting!|]
             return ()
         | otherwise -> do
             liftIO $ threadDelay 100000 -- Sleep 100ms
             loop


publishTree sf idToLabelAndVisibilityThreshold maybeMaxFailures topMessage elapsed tree = pbi
  where
    pbi = ProgressBarInfo { progressBarInfoTopMessage = T.pack <$> topMessage
                          , progressBarInfoBottomMessage = Just fullBottomMessage
                          , progressBarInfoSize = Just (100.0 * (fromIntegral (succeeded + pending + failed) / (fromIntegral total)))
                          , progressBarInfoAttachments = Nothing
                          , progressBarInfoBlocks = Just $ case maybeMaxFailures of
                              Nothing -> blocks
                              Just n -> L.take n blocks
                          }

    runningMessage = headMay $ L.sort $ catMaybes $ concatMap (extractValues (\node -> if isRunningItBlock node then Just $ runTreeLabel $ runNodeCommon node else Nothing)) tree

    fullBottomMessage = case runningMessage of
      Nothing -> bottomMessage
      Just t -> T.pack t <> "\n" <> bottomMessage
    bottomMessage = [i|#{succeeded} succeeded, #{failed} failed, #{pending} pending, #{totalRunningTests} running of #{total} (#{formatNominalDiffTime elapsed} elapsed)|]

    blocks = getFailureBlocks sf idToLabelAndVisibilityThreshold tree

    total = countWhere isItBlock tree
    succeeded = countWhere isSuccessItBlock tree
    pending = countWhere isPendingItBlock tree
    failed = countWhere isFailedItBlock tree
    totalRunningTests = countWhere isRunningItBlock tree
    -- totalNotStartedTests = countWhere isNotStartedItBlock tree

getFailureBlocks sf idToLabelAndVisibilityThreshold tree = catMaybes $ flip concatMap tree $ extractValuesControlRecurse $ \case
  RunNodeDescribe {} ->
    (True, Nothing) -- Recurse into grouping nodes, because their failures are actually just derived from child failures
  RunNodeParallel {} ->
    (True, Nothing)
  ((runTreeStatus . runNodeCommon) ->
    (Done {statusResult=(Failure (Pending {}))})) -> (False, Nothing)
  node@((runTreeStatus . runNodeCommon) -> (Done {statusResult=(Failure reason)})) | isFailedBlock node ->
    (False, Just $ getFailureBlock sf node reason)
  _ -> (True, Nothing)

  where
    getFailureBlock sf node reason = A.object [
      ("type", A.String "context")
      , ("elements", A.Array $ V.singleton $
            (A.object [("type", A.String "mrkdwn")
                      , ("text", A.String $
                                 ":red_circle: *"
                                 <> label
                                 <> "*"
                                 <> (if slackFormatterShowFailureReason sf then "\n" <> toMarkdown reason else "")
                                 <> (case failureCallStack reason of
                                        Just cs -> callStackToMarkdown (slackFormatterShowCallStacks sf) cs
                                        _ -> "")
                        )
                      ])
        )
      ]
      where
        -- Show a question mark if we can't determine the label for a node (should never happen).
        -- Otherwise, use slackFormatterVisibilityThreshold to filter if provided.
        filterFn k = case M.lookup k idToLabelAndVisibilityThreshold of
          Nothing -> Just "?"
          Just (l, thresh) -> case slackFormatterVisibilityThreshold sf of
            Just maxThresh | thresh > maxThresh -> Nothing
            _ -> Just l
        label = T.intercalate ", " $ mapMaybe filterFn $ toList $ runTreeAncestors $ runNodeCommon node

allIsDone :: [RunNodeFixed context] -> Bool
allIsDone = all (isDone . runTreeStatus . runNodeCommon)
  where
    isDone :: Status -> Bool
    isDone (Done {}) = True
    isDone _ = False
