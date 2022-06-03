{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The markdown summary report formatter appends Markdown summary information to a given file.
--
-- This is a "secondary formatter," i.e. one that can run in the background while a "primary formatter" (such as the TerminalUI or Print formatters) monopolize the foreground.
--
-- Documentation can be found <https://codedownio.github.io/sandwich/docs/formatters/markdown_summary here>.

module Test.Sandwich.Formatters.MarkdownSummary (
  defaultMarkdownSummaryFormatter
  , MarkdownSummaryFormatter

  -- * Options
  , markdownSummaryPath
  , markdownSummarySuccessIcon
  , markdownSummaryFailureIcon
  ) where

import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.String.Interpolate
import Data.Text as T
import Data.Time
import System.IO
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Interpreters.RunTree.Util (waitForTree)
import Test.Sandwich.RunTree
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Util


data MarkdownSummaryFormatter = MarkdownSummaryFormatter {
  markdownSummaryPath :: FilePath
  , markdownSummarySuccessIcon :: Maybe Text
  , markdownSummaryFailureIcon :: Maybe Text
  } deriving (Show)

defaultMarkdownSummaryFormatter :: FilePath -> MarkdownSummaryFormatter
defaultMarkdownSummaryFormatter path = MarkdownSummaryFormatter {
  markdownSummaryPath = path
  , markdownSummarySuccessIcon = Just ":heavy_check_mark: "
  , markdownSummaryFailureIcon = Just ":x: "
  }

instance Formatter MarkdownSummaryFormatter where
  formatterName _ = "markdown-summary-formatter"
  runFormatter = run
  finalizeFormatter _ _ _ = return ()

run :: (MonadIO m, MonadLogger m, MonadCatch m) => MarkdownSummaryFormatter -> [RunNode BaseContext] -> Maybe (CommandLineOptions ()) -> BaseContext -> m ()
run (MarkdownSummaryFormatter {..}) rts _ _bc = do
  let total = countWhere isItBlock rts

  startTime <- liftIO getCurrentTime

  mapM_ (liftIO . waitForTree) rts

  endTime <- liftIO getCurrentTime
  let timeDiff = formatNominalDiffTime $ diffUTCTime endTime startTime

  fixedTree <- liftIO $ atomically $ mapM fixRunTree rts
  let failed = countWhere isFailedItBlock fixedTree
  let pending = countWhere isPendingItBlock fixedTree

  liftIO $ withFile markdownSummaryPath AppendMode $ \h -> do
    if | failed == 0 -> do
           whenJust markdownSummarySuccessIcon (liftIO . (hPutStr h) . T.unpack)
           hPutStr h [i|All tests passed in #{timeDiff}.|]
       | otherwise -> do
           whenJust markdownSummaryFailureIcon (liftIO . (hPutStr h) . T.unpack)
           hPutStr h [i|#{failed} failed of #{total} in #{timeDiff}.|]
    case pending of
      0 -> hPutStrLn h ""
      _ -> hPutStrLn h [i| (#{pending} pending)|]
