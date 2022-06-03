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

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State
import Data.String.Interpolate
import Data.Text as T
import System.IO
import Test.Sandwich.Interpreters.RunTree.Util (waitForTree)
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


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
  runFormatter _ _ _ _ = return ()
  finalizeFormatter = printMarkdownSummary

printMarkdownSummary :: (MonadIO m, MonadLogger m, MonadCatch m) => MarkdownSummaryFormatter -> [RunNode BaseContext] -> BaseContext -> m ()
printMarkdownSummary msf@(MarkdownSummaryFormatter {..}) rts _bc = do
  Info {..} <- execStateT (mapM_ (runWithIndentation msf) rts) (Info 0 0 0)

  liftIO $ withFile markdownSummaryPath AppendMode $ \h -> do
    if | infoFailures == 0 -> hPutStrLn h ((maybe "" T.unpack markdownSummarySuccessIcon) <> [i|#{infoTotal} test#{if infoTotal > 1 then ("s" :: String) else ""} succeeded!|])
       | otherwise -> hPutStrLn h ((maybe "" T.unpack markdownSummaryFailureIcon) <> [i|#{infoFailures} test#{if infoFailures > 1 then ("s" :: String) else ""} failed of #{infoTotal}!|])

data Info = Info {
  infoSuccesses :: Int
  , infoFailures :: Int
  , infoTotal :: Int
  }

runWithIndentation :: (MonadIO m) => MarkdownSummaryFormatter -> RunNode context -> StateT Info m ()
runWithIndentation msf@(MarkdownSummaryFormatter {..}) node = do
  case node of
    RunNodeIt {} -> return ()
    RunNodeIntroduce {..} -> forM_ runNodeChildrenAugmented (runWithIndentation msf)
    RunNodeIntroduceWith {..} -> forM_ runNodeChildrenAugmented (runWithIndentation msf)
    _ -> forM_ (runNodeChildren node) (runWithIndentation msf)

  result <- liftIO $ waitForTree node

  -- Print the failure reason
  case node of
    RunNodeIt {} -> do
      modify (\x -> x { infoTotal = (infoTotal x) + 1 })
      case result of
        Success -> modify (\x -> x { infoTotal = (infoSuccesses x) + 1 })
        Cancelled -> modify (\x -> x { infoTotal = (infoFailures x) + 1 })
        Failure _ -> modify (\x -> x { infoTotal = (infoFailures x) + 1 })
        DryRun {} -> return ()
    _ -> return ()
