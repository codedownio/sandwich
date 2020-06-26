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
import qualified Data.List as L
import Data.String.Interpolate.IsString
import System.Console.ANSI
import Test.Sandwich.Formatters.Common.Count
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec

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

  runReaderT (mapM_ (runWithIndentation 0) rts) pf
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


runWithIndentation :: Int -> RunTree -> ReaderT PrintFormatter IO ()
runWithIndentation indent (RunTreeGroup {..}) = do
  size <- asks printFormatterIndentSize
  liftIO $ putStrLn [i|#{L.replicate (size * indent) ' '}#{runTreeLabel}|]
  forM_ runTreeChildren (runWithIndentation (indent + 1))
runWithIndentation indent (RunTreeSingle {..}) = do
  size <- asks printFormatterIndentSize
  liftIO $ wait runTreeAsync
  (liftIO $ readTVarIO runTreeStatus) >>= \case
    NotStarted -> return ()
    Running {} -> return ()
    Done {statusResult} -> do
      liftIO $ putStr $ L.replicate (size * indent) ' '
      case statusResult of
        Success -> printGreen runTreeLabel
        Failure (Pending _ maybeMessage) -> printYellow runTreeLabel
        Failure reason -> printRed runTreeLabel

-- * Printing with colors

printGreen, printYellow, printRed :: (MonadReader PrintFormatter m, MonadIO m) => String -> m ()
printGreen = printColor Dull Green
printYellow = printColor Dull Yellow
printRed = printColor Dull Red

printColor :: (MonadReader PrintFormatter m, MonadIO m) => ColorIntensity -> Color -> String -> m ()
printColor brightness color s = do
  useColor <- asks printFormatterUseColor
  when (useColor) $ liftIO $ setSGR [SetColor Foreground brightness color]
  liftIO $ putStrLn s
  when (useColor) $ liftIO $ setSGR [Reset]
