
module Test.Sandwich.Formatters.Slack.Internal.Markdown where

import Data.Function
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.Formatters.Slack.Internal.Types


toMarkdown :: FailureReason -> T.Text
toMarkdown (Reason {..}) = T.pack failureReason
toMarkdown (RawImage {..}) = T.pack failureFallback
toMarkdown (ChildrenFailed {failureNumChildren=n}) = [i|#{n} #{if n == 1 then ("child" :: T.Text) else "children"} failed|]
toMarkdown (ExpectedButGot {..}) = [i|Expected *#{failureValue1}* but got *#{failureValue2}*|]
toMarkdown (DidNotExpectButGot {..}) = [i|Did not expect *#{failureValue1}*|]
toMarkdown (GotException {..}) = case failureMessage of
  Just msg -> [i|Got exception (_#{msg}_): #{failureException}|]
  Nothing -> [i|Got exception (no message): #{failureException}|]
toMarkdown (Pending {}) = "Example was pending"
toMarkdown (GetContextException {..}) = [i|Context exception: #{failureException}|]
toMarkdown (GotAsyncException {..}) = case failureMessage of
  Just msg -> [i|Got async exception (_#{msg}_): #{failureAsyncException}|]
  Nothing -> [i|Got async exception: #{failureAsyncException}|]

callStackToMarkdown :: SlackFormatterShowCallStacks -> CallStack -> T.Text
callStackToMarkdown SlackFormatterNoCallStacks _cs = ""
callStackToMarkdown (SlackFormatterTopNCallStackFrames n) cs = "\n\n" <> showCallStack (fromCallSiteList $ L.take n $ getCallStack cs)
callStackToMarkdown SlackFormatterFullCallStack cs = "\n\n" <> showCallStack cs

showCallStack :: CallStack -> T.Text
showCallStack (getCallStack -> rows) = ["> *" <> (T.pack name) <> "*, called at "
                                        <> [i|_#{srcLocPackage}_:*#{srcLocFile}*:#{srcLocStartLine}:#{srcLocStartCol}|]
                                       | (name, SrcLoc {..}) <- rows]
  & T.intercalate "\n"
