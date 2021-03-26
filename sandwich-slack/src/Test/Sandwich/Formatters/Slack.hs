{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Sandwich.Formatters.Slack (
  SlackFormatter
  , SlackConfig(..)

  , defaultSlackFormatter

  , slackFormatterSlackConfig
  , slackFormatterChannel

  , slackFormatterTopMessage

  , slackFormatterMaxFailures
  , slackFormatterMaxFailureReasonLines
  , slackFormatterMaxCallStackLines

  , slackFormatterVisibilityThreshold

  , slackFormatterMaxMessageSize

  , SlackFormatterShowCallStacks(..)
  ) where

import Control.Applicative
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
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import GHC.Int
import Safe
import Test.Sandwich
import Test.Sandwich.Formatters.Internal.Markdown
import Test.Sandwich.Formatters.Internal.ProgressBar
import Test.Sandwich.Formatters.Internal.Types
import Test.Sandwich.Internal
import Test.Sandwich.Internal.Formatters
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree


data SlackFormatter = SlackFormatter {
  slackFormatterSlackConfig :: SlackConfig
  -- ^ Slack credentials
  , slackFormatterChannel :: String
  -- ^ Slack channel on which to create the progress bar.

  , slackFormatterTopMessage :: Maybe String
  -- ^ Message to put above the progress bar.
  -- For example, the name of the test suite and a link to the run in the CI system.

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

  , slackFormatterMaxMessageSize :: Maybe Int64
  -- ^ If present, make sure the messages we transmit to Slack default don't exceed this number
  -- of bytes. When a message does exceed it (probably because there are a ton of failures),
  -- start dropping blocks from the end of the message until the size is small enough.
  -- Making use of 'slackFormatterMaxFailures', 'slackFormatterMaxFailureReasonLines', and
  -- 'slackFormatterMaxCallStackLines' is a good way to avoid hitting the limit.
  }

defaultSlackFormatter :: SlackFormatter
defaultSlackFormatter = SlackFormatter {
  slackFormatterSlackConfig = SlackConfig "my-password"
  , slackFormatterChannel = "slack-channel"

  , slackFormatterTopMessage = Just "Top message"

  , slackFormatterMaxFailures = Just 30
  , slackFormatterMaxFailureReasonLines = Just 5
  , slackFormatterMaxCallStackLines = Just 5

  , slackFormatterVisibilityThreshold = Nothing

  -- 8KB, although Slack might accept 16KB now?
  , slackFormatterMaxMessageSize = Just 8192
  }

instance Formatter SlackFormatter where
  formatterName _ = "slack-formatter"
  runFormatter baseFormatter rts bc@(BaseContext {baseContextCommandLineOptions=(Just clo)}) =
    runApp (addCommandLineOptions clo baseFormatter) rts bc
  runFormatter baseFormatter rts bc@(BaseContext {baseContextCommandLineOptions=Nothing}) =
    runApp baseFormatter rts bc
  finalizeFormatter _ _ _ = return ()

addCommandLineOptions :: CommandLineOptions a -> SlackFormatter -> SlackFormatter
addCommandLineOptions (CommandLineOptions {optSlackOptions=(CommandLineSlackOptions {..})}) baseFormatter@(SlackFormatter {..}) = baseFormatter {
  slackFormatterSlackConfig = maybe slackFormatterSlackConfig (SlackConfig . T.pack) optSlackToken
  , slackFormatterChannel = fromMaybe slackFormatterChannel optSlackChannel
  , slackFormatterTopMessage = optSlackTopMessage <|> slackFormatterTopMessage
  , slackFormatterMaxFailures = optSlackMaxFailures <|> slackFormatterMaxFailures
  , slackFormatterMaxFailureReasonLines = optSlackMaxFailureReasonLines <|> slackFormatterMaxFailureReasonLines
  , slackFormatterMaxCallStackLines = optSlackMaxCallStackLines <|> slackFormatterMaxCallStackLines
  , slackFormatterVisibilityThreshold = optSlackVisibilityThreshold <|> slackFormatterVisibilityThreshold
  , slackFormatterMaxMessageSize = optSlackMaxMessageSize <|> slackFormatterMaxMessageSize
  }

runApp :: (MonadIO m, MonadCatch m, MonadLogger m) => SlackFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
runApp sf@(SlackFormatter {..}) rts _bc = do
  startTime <- liftIO getCurrentTime

  let extractFromNode node = let RunNodeCommonWithStatus {..} = runNodeCommon node in (runTreeId, (T.pack runTreeLabel, runTreeVisibilityLevel))
  let idToLabelAndVisibilityThreshold = M.fromList $ mconcat [extractValues extractFromNode node | node <- rts]

  rtsFixed <- liftIO $ atomically $ mapM fixRunTree rts
  let pbi = publishTree sf idToLabelAndVisibilityThreshold 0 rtsFixed
  pb <- (liftIO $ createProgressBar slackFormatterSlackConfig (T.pack slackFormatterChannel) slackFormatterMaxMessageSize pbi) >>= \case
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
      tryAny (liftIO $ updateProgressBar slackFormatterSlackConfig slackFormatterMaxMessageSize pb pbi') >>= \case
        Left err -> logError [i|Error updating progress bar: '#{err}'|]
        Right (Left err) -> logError [i|Inner error updating progress bar: '#{err}'. Blocks were '#{A.encode $ progressBarInfoBlocks pbi'}'|]
        Right (Right ()) -> return ()

      if | allIsDone newFixedTree -> do
             debug [i|All tree nodes are done, exiting!|]
             return ()
         | otherwise -> do
             liftIO $ threadDelay 100000 -- Sleep 100ms
             loop


publishTree sf idToLabelAndVisibilityThreshold elapsed tree = pbi
  where
    pbi = ProgressBarInfo {
      progressBarInfoTopMessage = T.pack <$> (slackFormatterTopMessage sf)
      , progressBarInfoBottomMessage = Just fullBottomMessage
      , progressBarInfoSize = Just (100.0 * (fromIntegral (succeeded + pending + failed) / (fromIntegral total)))
      , progressBarInfoAttachments = Nothing
      , progressBarInfoBlocks = Just $ case slackFormatterMaxFailures sf of
          Nothing -> mconcat blocks
          Just n -> case L.splitAt n blocks of
            (xs, []) -> mconcat xs
            (xs, rest) -> mconcat xs <> [extraFailuresBlock (L.length rest)]
      }

    runningMessage = headMay $ L.sort $ catMaybes $ flip concatMap tree $
      extractValues (\node -> if isRunningItBlock node then Just $ runTreeLabel $ runNodeCommon node else Nothing)

    fullBottomMessage = case runningMessage of
      Nothing -> bottomMessage
      Just t -> T.pack t <> "\n" <> bottomMessage
    bottomMessage = [i|#{succeeded} succeeded, #{failed} failed, #{pending} pending, #{totalRunningTests} running of #{total} (#{formatNominalDiffTime elapsed} elapsed)|]

    blocks = catMaybes $ flip concatMap tree $ extractValuesControlRecurse $ \case
      -- Recurse into grouping nodes, because their failures are actually just derived from child failures
      RunNodeDescribe {} -> (True, Nothing)
      RunNodeParallel {} -> (True, Nothing)
      ((runTreeStatus . runNodeCommon) -> (Done {statusResult=(Failure (Pending {}))})) -> (False, Nothing)
      node@((runTreeStatus . runNodeCommon) -> (Done {statusResult=(Failure reason)})) | isFailedBlock node ->
        (False, Just $ singleFailureBlocks sf idToLabelAndVisibilityThreshold node reason)
      _ -> (True, Nothing)

    total = countWhere isItBlock tree
    succeeded = countWhere isSuccessItBlock tree
    pending = countWhere isPendingItBlock tree
    failed = countWhere isFailedItBlock tree
    totalRunningTests = countWhere isRunningItBlock tree
    -- totalNotStartedTests = countWhere isNotStartedItBlock tree


singleFailureBlocks sf idToLabelAndVisibilityThreshold node reason = catMaybes [
  Just $ markdownSectionWithLines [":red_circle: *" <> label <> "*"]

  -- Failure reason info
  , case (markdownLinesToShow, _overflowMarkdownLines) of
      ([], _) -> Nothing
      (toShow, []) -> Just $ markdownSectionWithLines toShow
      (toShow, overflow) -> Just $ markdownSectionWithLines $ addToLastLine toShow [i| (+ #{L.length overflow} more lines)|]

  -- Callstack info
  , case (callStackLinesToShow, _overflowCallStackLines) of
      ([], _) -> Nothing
      (toShow, []) -> Just $ markdownSectionWithLines toShow
      (toShow, overflow) -> Just $ markdownSectionWithLines $ addToLastLine toShow [i| (+ #{L.length overflow} more lines)|]
  ]
  where
    allMarkdownLines = T.lines $ toMarkdown reason
    (markdownLinesToShow, _overflowMarkdownLines) = case slackFormatterMaxFailureReasonLines sf of
      Nothing -> (allMarkdownLines, [])
      Just n -> L.splitAt n allMarkdownLines

    allCallStackLines = case failureCallStack reason of
      Just cs -> L.filter (not . T.null) $ T.lines $ callStackToMarkdown SlackFormatterFullCallStack cs
      _ -> []
    (callStackLinesToShow, _overflowCallStackLines) = case slackFormatterMaxCallStackLines sf of
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

extraFailuresBlock numExtraFailures = markdownSectionWithLines [[i|+ #{numExtraFailures} more failure|]]

markdownBlockWithLines ls = A.object [("type", A.String "mrkdwn"), ("text", A.String $ T.unlines ls)]

markdownSectionWithLines ls = A.object [("type", A.String "section"), ("text", markdownBlockWithLines ls)]

addToLastLine :: [T.Text] -> T.Text -> [T.Text]
addToLastLine [] _ = []
addToLastLine xs toAdd = (init xs) <> [last xs <> toAdd]

allIsDone :: [RunNodeFixed context] -> Bool
allIsDone = all (isDone . runTreeStatus . runNodeCommon)
  where
    isDone :: Status -> Bool
    isDone (Done {}) = True
    isDone _ = False
