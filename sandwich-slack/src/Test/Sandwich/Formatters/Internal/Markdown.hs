{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
toMarkdown (Reason {..}) = T.pack failureReason
toMarkdown (ExpectedButGot {..}) = [i|Expected *#{failureValue1}* but got *#{failureValue2}*|]
toMarkdown (DidNotExpectButGot {..}) = [i|Did not expect *#{failureValue1}*|]
toMarkdown (GotException {..}) = case failureMessage of
  Just msg -> [i|Got exception (_#{msg}_): #{failureException}|]
  Nothing -> [i|Got exception: #{failureException}|]
toMarkdown (Pending {..}) = "Example was pending"
toMarkdown (GetContextException {..}) = [i|Context exception: #{failureException}|]
toMarkdown (GotAsyncException {..}) = case failureMessage of
  Just msg -> [i|Got async exception (_#{msg}_): #{failureAsyncException}|]
  Nothing -> [i|Got async exception: #{failureAsyncException}|]

callStackToMarkdown :: SlackFormatterShowCallStacks -> CallStack -> T.Text
callStackToMarkdown SlackFormatterNoCallStacks cs = ""
callStackToMarkdown (SlackFormatterTopNCallStackFrames n) cs = "\n\n" <> showCallStack (fromCallSiteList $ L.take n $ getCallStack cs)
callStackToMarkdown SlackFormatterFullCallStack cs = "\n\n" <> showCallStack cs

showCallStack (getCallStack -> rows) = ["> *" <> (T.pack name) <> "*, called at "
                                        <> [i|_#{srcLocPackage}_:*#{srcLocFile}*:#{srcLocStartLine}:#{srcLocStartCol}|]
                                       | (name, SrcLoc {..}) <- rows]
  & T.intercalate "\n"
