-- | Pretty printing failure reasons

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sandwich.Formatters.Print.FailureReason (
  printFailureReason
  ) where

import Control.Exception.Safe
import Control.Monad.Reader
import qualified Data.List as L
import Data.String.Interpolate.IsString
import System.IO
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.PrintPretty
import Test.Sandwich.Formatters.Print.Printing as P
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.Spec
import Text.Show.Pretty as P


printFailureReason :: FailureReason -> ReaderT (PrintFormatter, Int, Handle) IO ()
printFailureReason (Reason _ s) = do
  printShowBoxPrettyWithTitle "Reason: " (SEB s)
printFailureReason (ExpectedButGot _ seb1 seb2) = do
  printShowBoxPrettyWithTitle "Expected: " seb1
  printShowBoxPrettyWithTitle "But got: " seb2
printFailureReason (DidNotExpectButGot _ seb) = do
  printShowBoxPrettyWithTitle "Did not expect: " seb
printFailureReason (GotException _ maybeMessage e@(SomeExceptionWithEq baseException)) =
  case fromException baseException of
    Just (fr :: FailureReason) -> do
      picn midWhite "Got exception:"
      printFailureReason fr
    _ -> case maybeMessage of
      Nothing -> printShowBoxPrettyWithTitle "Got exception: " (SEB e)
      Just s -> printShowBoxPrettyWithTitle [i|Got exception (#{s})|] (SEB e)
printFailureReason (Pending _ maybeMessage) = case maybeMessage of
  Nothing -> return () -- Just allow the yellow heading to show the pending state
  Just s -> printShowBoxPrettyWithTitle "Pending reason: " (SEB s)
printFailureReason (GetContextException _ e@(SomeExceptionWithEq baseException)) = do
  case fromException baseException of
    Just (fr :: FailureReason) -> do
      picn midWhite "Context exception:"
      printFailureReason fr
    _ -> printShowBoxPrettyWithTitle "Context exception: " (SEB e)
printFailureReason (GotAsyncException _ maybeMessage e) = case maybeMessage of
  Nothing -> printShowBoxPrettyWithTitle "Async exception" (SEB e)
  Just s -> printShowBoxPrettyWithTitle [i|Async exception (#{e}) |] (SEB s)

-- * Pretty printing

printShowBoxPrettyWithTitle :: String -> ShowEqBox -> ReaderT (PrintFormatter, Int, Handle) IO ()
printShowBoxPrettyWithTitle title (SEB v) = case P.reify v of
  Nothing -> do
    picn midWhite title
    withBumpIndent $ do
      forM_ (L.lines $ show v) pin
    p "\n"
  Just x
    | isSingleLine x -> do
        pic midWhite title
        printPretty False x >> p "\n"
    | otherwise -> do
        picn midWhite title
        printPretty True x >> p "\n"

-- printShowBoxPretty (SEB v) = case P.reify v of
--   Nothing -> forM_ (L.lines $ show v) pin
--   Just x -> printPretty True x >> p "\n"
