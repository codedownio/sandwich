{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.Formatters.Slack (
  SlackFormatter
  , SlackConfig(..)

  , defaultSlackFormatter
  , slackFormatterSlackConfig
  , slackFormatterTopMessage
  , slackFormatterChannel
  , slackFormatterMaxFailures
  , slackFormatterMaxFailureReasonLines
  , slackFormatterMaxCallStackLines
  , slackFormatterVisibilityThreshold

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

  , slackFormatterMaxFailureReasonLines :: Maybe Int
  -- ^ Maximum number of lines to devote to showing the failure reason underneath a failure.
  -- Set to 'Just 0' to disable showing failure reasons.

  , slackFormatterMaxCallStackLines :: Maybe Int
  -- ^ Maximum number of lines to devote to showing the call stack underneath a failure.
  -- Set to 'Just 0' to disable showing call stacks.

  , slackFormatterVisibilityThreshold :: Maybe Int
  -- ^ If present, filter the headings on failures to only include nodes whose visibility
  -- threshold is less than or equal to the value.
  }

defaultSlackFormatter :: SlackFormatter
defaultSlackFormatter = SlackFormatter {
  slackFormatterSlackConfig = SlackConfig "my-password"
  , slackFormatterTopMessage = Just "Top message"
  , slackFormatterChannel = "slack-channel"

  , slackFormatterMaxFailures = Just 30
  , slackFormatterMaxFailureReasonLines = Just 5
  , slackFormatterMaxCallStackLines = Just 5

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
  let pbi = publishTree sf idToLabelAndVisibilityThreshold 0 rtsFixed
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
      let pbi' = publishTree sf idToLabelAndVisibilityThreshold (diffUTCTime now startTime) newFixedTree
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


publishTree sf idToLabelAndVisibilityThreshold elapsed tree = pbi
  where
    pbi = ProgressBarInfo { progressBarInfoTopMessage = T.pack <$> (slackFormatterTopMessage sf)
                          , progressBarInfoBottomMessage = Just fullBottomMessage
                          , progressBarInfoSize = Just (100.0 * (fromIntegral (succeeded + pending + failed) / (fromIntegral total)))
                          , progressBarInfoAttachments = Nothing
                          , progressBarInfoBlocks = Just $ case slackFormatterMaxFailures sf of
                              Nothing -> mconcat blocks
                              Just n -> case L.splitAt n blocks of
                                (xs, []) -> mconcat xs
                                (xs, rest) -> mconcat xs <> [extraFailuresBlock (L.length rest)]
                          }

    runningMessage = headMay $ L.sort $ catMaybes $ concatMap (extractValues (\node -> if isRunningItBlock node then Just $ runTreeLabel $ runNodeCommon node else Nothing)) tree

    fullBottomMessage = case runningMessage of
      Nothing -> bottomMessage
      Just t -> T.pack t <> "\n" <> bottomMessage
    bottomMessage = [i|#{succeeded} succeeded, #{failed} failed, #{pending} pending, #{totalRunningTests} running of #{total} (#{formatNominalDiffTime elapsed} elapsed)|]

    blocks = flip concatMap tree $ extractValuesControlRecurse $ \case
      -- Recurse into grouping nodes, because their failures are actually just derived from child failures
      RunNodeDescribe {} -> (True, [])
      RunNodeParallel {} -> (True, [])
      ((runTreeStatus . runNodeCommon) ->
        (Done {statusResult=(Failure (Pending {}))})) -> (False, [])
      node@((runTreeStatus . runNodeCommon) -> (Done {statusResult=(Failure reason)})) | isFailedBlock node ->
        (False, singleFailureBlocks sf idToLabelAndVisibilityThreshold node reason)
      _ -> (True, [])

    total = countWhere isItBlock tree
    succeeded = countWhere isSuccessItBlock tree
    pending = countWhere isPendingItBlock tree
    failed = countWhere isFailedItBlock tree
    totalRunningTests = countWhere isRunningItBlock tree
    -- totalNotStartedTests = countWhere isNotStartedItBlock tree


singleFailureBlocks sf idToLabelAndVisibilityThreshold node reason = catMaybes [
  Just $ markdownSectionWithLines [":red_circle: *" <> label <> "*"]

  -- Failure reason info
  , case (markdownLinesToShow, overflowMarkdownLines) of
      ([], _) -> Nothing
      (toShow, []) -> Just $ markdownSectionWithLines toShow
      (toShow, overflow) -> Just $ A.object [
        ("type", A.String "section")
        , ("text", markdownBlockWithLines toShow)
        , ("accessory", A.object [("type", "overflow"), ("options", A.Array (V.fromList [markdownBlockWithLines overflow]))])
        ]

  -- Callstack info
  , case (callStackLinesToShow, overflowCallStackLines) of
      ([], _) -> Nothing
      (toShow, []) -> Just $ markdownSectionWithLines toShow
      (toShow, overflow) -> Just $ A.object [
        ("type", A.String "section")
        , ("text", markdownBlockWithLines toShow)
        , ("accessory", A.object [("type", "overflow"), ("options", A.Array (V.fromList [markdownBlockWithLines overflow]))])
        ]
  ]
  where
    allMarkdownLines = T.lines $ toMarkdown reason
    (markdownLinesToShow, overflowMarkdownLines) = case slackFormatterMaxFailureReasonLines sf of
      Nothing -> (allMarkdownLines, [])
      Just n -> L.splitAt n allMarkdownLines

    allCallStackLines = case failureCallStack reason of
      Just cs -> L.filter (not . T.null) $ T.lines $ callStackToMarkdown SlackFormatterFullCallStack cs
      _ -> []
    (callStackLinesToShow, overflowCallStackLines) = case slackFormatterMaxCallStackLines sf of
      Nothing -> (allCallStackLines, [])
      Just n -> L.splitAt n allCallStackLines

    -- Show a question mark if we can't determine the label for a node (should never happen).
    -- Otherwise, use slackFormatterVisibilityThreshold to filter if provided.
    filterFn k = case M.lookup k idToLabelAndVisibilityThreshold of
      Nothing -> Just "?"
      Just (l, thresh) -> case slackFormatterVisibilityThreshold sf of
        Just maxThresh | thresh > maxThresh -> Nothing
        _ -> Just l
    label = T.intercalate ", " $ mapMaybe filterFn $ toList $ runTreeAncestors $ runNodeCommon node

extraFailuresBlock numExtraFailures = markdownSectionWithLines [[i|+ #{numExtraFailures} more|]]

markdownBlockWithLines ls = A.object [("type", A.String "mrkdwn"), ("text", A.String $ T.unlines ls)]

markdownSectionWithLines ls = A.object [("type", A.String "section"), ("text", markdownBlockWithLines ls)]


allIsDone :: [RunNodeFixed context] -> Bool
allIsDone = all (isDone . runTreeStatus . runNodeCommon)
  where
    isDone :: Status -> Bool
    isDone (Done {}) = True
    isDone _ = False
