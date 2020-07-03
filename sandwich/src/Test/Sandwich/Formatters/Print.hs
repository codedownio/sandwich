{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
-- |

module Test.Sandwich.Formatters.Print (
  defaultPrintFormatter
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import qualified Data.List as L
import Data.String.Interpolate.IsString
import Graphics.Vty
import System.Console.ANSI
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.PrintPretty
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util
import Text.Show.Pretty as P

instance Formatter PrintFormatter where
  runFormatter = runApp

runApp :: PrintFormatter -> [RunTree] -> IO ()
runApp pf@(PrintFormatter {..}) rts = do
  let total = countWhere isItBlock rts

  putStrLn "\n"
  putStrLn [i|Beginning suite of #{total} tests\n|]

  runReaderT (mapM_ runWithIndentation rts) (pf, 1)
  putStrLn "\n"

  fixedTree <- atomically $ mapM fixRunTree rts
  let succeeded = countWhere isSuccessItBlock fixedTree
  let failed = countWhere isFailedItBlock fixedTree
  let pending = countWhere isPendingItBlock fixedTree
  let total = countWhere isItBlock fixedTree

  if | failed == 0 -> putStr [i|All tests passed!|]
     | otherwise -> putStr [i|#{failed} failed of #{total}.|]
  case pending of
    0 -> putStrLn ""
    _ -> putStrLn [i| (#{pending} pending)|]


runWithIndentation :: RunTree -> ReaderT (PrintFormatter, Int) IO ()
runWithIndentation (RunTreeGroup {..}) = do
  p runTreeLabel
  withBumpIndent $ forM_ runTreeChildren runWithIndentation
runWithIndentation (RunTreeSingle {..}) = do
  liftIO $ wait runTreeAsync
  (liftIO $ readTVarIO runTreeStatus) >>= \case
    NotStarted -> return ()
    Running {} -> return ()
    Done {statusResult} -> do
      case statusResult of
        Success -> pGreenLn runTreeLabel
        Failure (Pending _ maybeMessage) -> pYellowLn runTreeLabel
        Failure reason -> do
          pRedLn runTreeLabel
          withBumpIndent $ printFailureReason reason

-- * Pretty printing failure reason

printFailureReason (Reason maybeCallStack s) = do
  printShowBoxPrettyWithTitle "Reason: " (SEB s)
printFailureReason (ExpectedButGot maybeCallStack seb1 seb2) = do
  printShowBoxPrettyWithTitle "Expected: " seb1
  printShowBoxPrettyWithTitle "But got: " seb2
printFailureReason (DidNotExpectButGot maybeCallStack seb) = do
  printShowBoxPrettyWithTitle "Did not expect: " seb
printFailureReason (GotException maybeCallStack e) = do
  printShowBoxPrettyWithTitle "Got exception: " (SEB e)
printFailureReason (Pending maybeCallStack maybeMessage) = case maybeMessage of
  Nothing -> return () -- Just allow the yellow heading to show the pending state
  Just s -> printShowBoxPrettyWithTitle "Pending reason: " (SEB s)
printFailureReason (GetContextException e) = do
  printShowBoxPrettyWithTitle "Got exception: " (SEB e)
printFailureReason (GotAsyncException maybeMessage e) = case maybeMessage of
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
