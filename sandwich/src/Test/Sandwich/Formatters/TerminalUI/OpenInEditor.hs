{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |

module Test.Sandwich.Formatters.TerminalUI.OpenInEditor where

import Control.Monad
import Data.String.Interpolate.IsString
import GHC.Stack
import System.Environment
import System.Process
import qualified Data.Text as T

autoOpenInEditor :: SrcLoc -> IO ()
autoOpenInEditor loc = do
  lookupEnv "EDITOR" >>= \case
    Nothing -> return ()
    Just s | "emacs" `T.isInfixOf` (T.toLower $ T.pack s) -> openInEmacs loc
    Just s -> return () -- TODO: support vim, VSCode, etc.

openInEmacs :: SrcLoc -> IO ()
openInEmacs (SrcLoc {..}) = do
  void $ createProcess $ (proc "emacsclient" [[i|+#{srcLocStartLine}:#{srcLocStartCol}|], srcLocFile, "--no-wait"]) {std_out=CreatePipe, std_err=CreatePipe}
  void $ createProcess $ (proc "emacsclient" ["--eval", elisp, "--no-wait"]) {std_out=CreatePipe, std_err=CreatePipe}
    where
      elisp = [i|(progn
                   (x-focus-frame (selected-frame))
                   (raise-frame)
                   )|]
