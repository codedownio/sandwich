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

withBumpIndent action = do
  (PrintFormatter {..}, indent) <- ask
  withBumpIndent' printFormatterIndentSize action
withBumpIndent' n = local (\(pf, indent) -> (pf, indent + n))

printFailureReason (ExpectedButGot maybeCallStack seb1 seb2) = do
  printShowBoxPrettyWithTitle "Expected: " seb1
  -- p "\n"
  printShowBoxPrettyWithTitle "But got: " seb2
printFailureReason reason = do
  p (show reason)



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

printPretty (getPrintFn -> f) (Quote s) = f (quoteColor) s
printPretty (getPrintFn -> f) (Time s) = f (timeColor) s
printPretty (getPrintFn -> f) (Date s) = f (dateColor) s
printPretty (getPrintFn -> f) (String s) = f (stringColor) s
printPretty (getPrintFn -> f) (Char s) = f (charColor) s
printPretty (getPrintFn -> f) (Float s) = f (floatColor) s
printPretty (getPrintFn -> f) (Integer s) = f (integerColor) s
printPretty indentFirst (Rec name tuples) = do
  (if indentFirst then pic else pc) recordNameColor name
  pcn braceColor " {"
  withBumpIndent $
    forM_ tuples $ \(name, val) -> do
      pic fieldNameColor name
      p " = "
      withBumpIndent' (L.length name + L.length " = ") $ do
        printPretty False val
        p "\n"
  pic braceColor "}"
printPretty indentFirst (Con name values) = do
  (if indentFirst then pic else pc) constructorNameColor (name <> " ")
  case values of
    [] -> return ()
    (x:xs) -> do
      printPretty False x
      p "\n"
      withBumpIndent' (L.length name + L.length " ") $ do
        sequence_ (L.intercalate [p "\n"] [[printPretty True v] | v <- xs])
printPretty indentFirst (List values) = printListWrappedIn ("[", "]") indentFirst values
printPretty indentFirst (Tuple values) = printListWrappedIn ("(", ")") indentFirst values
printPretty indentFirst x = pin $ show x

-- printPretty (Ratio v1 v2) = printIndentedWithRGBColor (Just (ratioColor))
-- printPretty (Neg s) = printIndentedWithRGBColor (Just (negColor)) s

printListWrappedIn (begin, end) (getPrintFn -> f) values | all isSingleLine values = do
  f listBracketColor begin
  sequence_ (L.intercalate [p ", "] [[printPretty False v] | v <- values])
  pc listBracketColor end
printListWrappedIn (begin, end) (getPrintFn -> f) values = do
  f listBracketColor begin
  p "\n"
  withBumpIndent $ do
    forM_ values $ \v -> do
      printPretty True v
      p "\n"
  pic listBracketColor end

getPrintFn True = pic
getPrintFn False = pc
