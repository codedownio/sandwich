{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Pretty printing failure reason

module Test.Sandwich.Formatters.Print.FailureReason (
  printFailureReason
  ) where

import Control.Concurrent
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import qualified Data.List as L
import qualified Data.Sequence as Seq
import Data.String.Interpolate.IsString
import Graphics.Vty
import System.Console.ANSI
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.Logs
import Test.Sandwich.Formatters.Print.PrintPretty
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util
import Text.Show.Pretty as P


printFailureReason (Reason maybeCallStack s) = do
  printShowBoxPrettyWithTitle "Reason: " (SEB s)
printFailureReason (ExpectedButGot maybeCallStack seb1 seb2) = do
  printShowBoxPrettyWithTitle "Expected: " seb1
  printShowBoxPrettyWithTitle "But got: " seb2
printFailureReason (DidNotExpectButGot maybeCallStack seb) = do
  printShowBoxPrettyWithTitle "Did not expect: " seb
printFailureReason (GotException maybeCallStack maybeMessage e) = case maybeMessage of
  Nothing -> printShowBoxPrettyWithTitle "Got exception: " (SEB e)
  Just s -> printShowBoxPrettyWithTitle [i|Got exception (#{s}): |] (SEB s)
printFailureReason (Pending maybeCallStack maybeMessage) = case maybeMessage of
  Nothing -> return () -- Just allow the yellow heading to show the pending state
  Just s -> printShowBoxPrettyWithTitle "Pending reason: " (SEB s)
printFailureReason (GetContextException maybeCallStack e) = do
  printShowBoxPrettyWithTitle "Got exception: " (SEB e)
printFailureReason (GotAsyncException maybeCallStack maybeMessage e) = case maybeMessage of
  Nothing -> printShowBoxPrettyWithTitle "Async exception" (SEB e)
  Just s -> printShowBoxPrettyWithTitle [i|Async exception (#{e}) |] (SEB s)

-- * Pretty printing

printShowBoxPrettyWithTitle :: String -> ShowEqBox -> ReaderT (PrintFormatter, Int) IO ()
printShowBoxPrettyWithTitle title (SEB v) = case P.reify v of
  Nothing -> do
    picn midWhite title
    withBumpIndent $ p $ show v
  Just x
    | isSingleLine x -> do
        pic midWhite title
        printPretty False x >> p "\n"
    | otherwise -> do
        picn midWhite title
        printPretty True x >> p "\n"

printShowBoxPretty (SEB v) = case P.reify v of
  Nothing -> p $ show v
  Just x -> printPretty True x >> p "\n"
