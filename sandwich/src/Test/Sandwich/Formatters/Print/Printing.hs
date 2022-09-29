
-- | Utility functions for printing

module Test.Sandwich.Formatters.Print.Printing where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.List as L
import System.Console.ANSI
import System.IO
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Util


-- * Printing functions for indented, colored, and with newline

pi msg = printIndentedWithColor Nothing msg
pic color msg = printIndentedWithColor (Just (SetRGBColor Foreground color)) msg
pin msg = printIndentedWithColor Nothing (msg <> "\n")
picn color msg = printIndentedWithColor (Just (SetRGBColor Foreground color)) (msg <> "\n")

p msg = printWithColor Nothing msg
pc color msg = printWithColor (Just (SetRGBColor Foreground color)) msg
pn msg = printWithColor Nothing (msg <> "\n")
pcn color msg = printWithColor (Just (SetRGBColor Foreground color)) (msg <> "\n")

pGreenLn msg = printIndentedWithColor (Just (SetColor Foreground Dull Green)) (msg <> "\n")
pYellowLn msg = printIndentedWithColor (Just (SetColor Foreground Dull Yellow)) (msg <> "\n")
pRedLn msg = printIndentedWithColor (Just (SetColor Foreground Dull Red)) (msg <> "\n") -- Tried solarizedRed here but it was too orange

printIndentedWithColor maybeColor msg = do
  (PrintFormatter {}, indent, h) <- ask
  liftIO $ hPutStr h $ L.replicate indent ' '
  printWithColor maybeColor msg

printWithColor maybeColor msg = do
  (PrintFormatter {printFormatterUseColor}, _, h) <- ask
  when (printFormatterUseColor) $ whenJust maybeColor $ \color -> liftIO $ setSGR [color]
  liftIO $ hPutStr h msg
  when (printFormatterUseColor) $ whenJust maybeColor $ \_ -> liftIO $ setSGR [Reset]
