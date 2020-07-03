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
import System.Console.ANSI
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Formatters.Print.Color
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util
import Text.Show.Pretty as P

data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  , printFormatterIncludeLogs :: Bool
  , printFormatterIndentSize :: Int
  }

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterIncludeLogs = True
  , printFormatterIndentSize = 4
  }

instance Formatter PrintFormatter where
  runFormatter = runApp

runApp :: PrintFormatter -> [RunTree] -> IO ()
runApp pf@(PrintFormatter {..}) rts = do
  let total = countWhere isItBlock rts

  putStrLn "\n"
  putStrLn [i|Beginning suite of #{total} tests\n|]

  runReaderT (mapM_ runWithIndentation rts) (pf, 0)
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
        Success -> pGreen runTreeLabel
        Failure (Pending _ maybeMessage) -> pYellow runTreeLabel
        Failure reason -> do
          pRed runTreeLabel
          withBumpIndent $ printFailureReason reason

withBumpIndent = local (\(pf, indent) -> (pf, indent + 1))

printFailureReason (ExpectedButGot maybeCallStack seb1 seb2) = do
  pRGB midWhite "Expected:"
  withBumpIndent $ printShowBoxPretty seb1
  pRGB midWhite "But got:"
  withBumpIndent $ printShowBoxPretty seb2
printFailureReason reason = do
  p (show reason)

-- * Printing indented

p msg = printIndentedWithColor Nothing msg

pRGB color msg = printIndentedWithRGBColor (Just color) msg

pGreen = printIndentedWithColor (Just (Dull, Green))
pYellow = printIndentedWithColor (Just (Dull, Yellow))
pRed = printIndentedWithColor (Just (Dull, Red)) -- Tried solarizedRed here but it was too orange

printIndentedWithColor maybeColor msg = do
  (PrintFormatter {..}, indent) <- ask
  liftIO $ putStr $ L.replicate (printFormatterIndentSize * indent) ' '
  when (printFormatterUseColor) $ whenJust maybeColor $ \(brightness, color) ->
    liftIO $ setSGR [SetColor Foreground brightness color]
  liftIO $ putStrLn msg
  when (printFormatterUseColor) $ whenJust maybeColor $ \_ -> liftIO $ setSGR [Reset]

printIndentedWithRGBColor maybeColor msg = do
  (PrintFormatter {..}, indent) <- ask
  liftIO $ putStr $ L.replicate (printFormatterIndentSize * indent) ' '
  when (printFormatterUseColor) $ whenJust maybeColor $ \color ->
    liftIO $ setSGR [SetRGBColor Foreground color]
  liftIO $ putStrLn msg
  when (printFormatterUseColor) $ whenJust maybeColor $ \_ -> liftIO $ setSGR [Reset]

-- * Pretty printing

printShowBoxPretty (SEB v) = case P.reify v of
  Nothing -> p $ show v
  Just x -> printPretty x

printPretty (Quote s) = pRGB (quoteColor) s
printPretty (Time s) = pRGB (timeColor) s
printPretty (Date s) = pRGB (dateColor) s
printPretty (String s) = pRGB (stringColor) s
printPretty (Char s) = pRGB (charColor) s
printPretty (Float s) = pRGB (floatColor) s
printPretty (Integer s) = pRGB (integerColor) s
printPretty x = p $ show x
-- printPretty (Ratio v1 v2) = printIndentedWithRGBColor (Just (ratioColor))
-- printPretty (Neg s) = printIndentedWithRGBColor (Just (negColor)) s
