{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- |

module Test.Sandwich.Formatters.Print.PrintPretty (
  printPretty
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
import System.Console.ANSI
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.Print.Util
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util
import Text.Show.Pretty as P


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
      withBumpIndent' (L.length name + L.length (" = " :: String)) $ do
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
      withBumpIndent' (L.length name + L.length (" " :: String)) $ do
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
