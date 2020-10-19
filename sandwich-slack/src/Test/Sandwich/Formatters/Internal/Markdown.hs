{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Sandwich.Formatters.Internal.Markdown where

import qualified Data.Aeson as A
import Data.Function
import qualified Data.List as L
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Formatters.Internal.Types


toMarkdown :: FailureReason -> T.Text
toMarkdown (Reason {..}) = markdownBlock (T.pack failureReason)
toMarkdown (ExpectedButGot {..}) = markdownBlock [i|Expected *#{failureValue1}* but got *#{failureValue2}*|]
toMarkdown (DidNotExpectButGot {..}) = markdownBlock [i|Did not expect *#{failureValue1}*|]
toMarkdown (GotException {..}) = ""
toMarkdown (Pending {..}) = ""
toMarkdown (GetContextException {..}) = ""
toMarkdown (GotAsyncException {..}) = ""



callStackToMarkdown :: SlackFormatterShowCallStacks -> CallStack -> T.Text
callStackToMarkdown SlackFormatterNoCallStacks cs = ""
callStackToMarkdown (SlackFormatterTopNCallStackFrames n) cs = "\n\n" <> showCallStack (fromCallSiteList $ L.take n $ getCallStack cs)
callStackToMarkdown SlackFormatterFullCallStack cs = "\n\n" <> showCallStack cs


markdownBlock text = text

showCallStack cs = prettyCallStack cs
  & T.pack
  & T.lines
  & fmap ("> " <>)
  & T.intercalate "\n"

-- markdownBlock text = A.object [
--   ("type", A.String "mrkdwn")
--   , ("text", A.String $ "\n" <> text)
--   ]
