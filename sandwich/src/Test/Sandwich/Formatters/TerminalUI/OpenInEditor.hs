{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Sandwich.Formatters.TerminalUI.OpenInEditor where

import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import System.Environment
import System.Process

autoOpenInEditor :: SrcLoc -> IO ()
autoOpenInEditor loc = do
  editor <- lookupEnv "EDITOR"
  display <- lookupEnv "DISPLAY"
  case (editor, display) of
    (Nothing, _) -> return ()
    (Just s, display) | "emacs" `T.isInfixOf` (T.toLower $ T.pack s) -> openInEmacs loc (isJust display)
    (Just _, _) -> return () -- TODO: support vim, VSCode, etc.

openInEmacs :: SrcLoc -> Bool -> IO ()
openInEmacs (SrcLoc {..}) hasDisplay = do
  void $ createProcess $ (proc "emacsclient" (nwArg <> [[i|+#{srcLocStartLine}:#{srcLocStartCol}|], srcLocFile, "--no-wait"])) {std_out=CreatePipe, std_err=CreatePipe}
  void $ createProcess $ (proc "emacsclient" (nwArg <> ["--eval", elisp, "--no-wait"])) {std_out=CreatePipe, std_err=CreatePipe}
    where
      nwArg = if hasDisplay then [] else ["-nw"]

      elisp = [i|(progn
                   (x-focus-frame (selected-frame))
                   (raise-frame)
                   (recenter)
                   )|]
