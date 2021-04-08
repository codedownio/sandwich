{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Sandwich.Formatters.TerminalUI.OpenInEditor where

import Control.Applicative
import Data.Function
import qualified Data.Text as T
import GHC.Stack
import System.Environment
import System.Exit
import System.Process


autoOpenInEditor :: Maybe String -> (T.Text -> IO ()) -> SrcLoc -> IO ()
autoOpenInEditor terminalUIDefaultEditor debugFn (SrcLoc {..}) = do
  maybeEditor' <- lookupEnv "EDITOR"
  let maybeEditor = maybeEditor' <|> terminalUIDefaultEditor

  case maybeEditor of
    Nothing -> return ()
    Just editorString -> do
      let editor = editorString
                 & T.pack
                 & T.replace "LINE" (T.pack $ show srcLocStartLine)
                 & T.replace "COLUMN" (T.pack $ show srcLocStartCol)
                 & fillInFile
                 & T.unpack

      debugFn ("Opening editor with command: " <> T.pack editor)

      (_, _, _, p) <- createProcess ((shell editor) { delegate_ctlc = True })
      waitForProcess p >>= \case
        ExitSuccess -> return ()
        ExitFailure n -> debugFn ("Editor failed with exit code " <> T.pack (show n))

  where
    fillInFile cmd
      | "FILE" `T.isInfixOf` cmd = T.replace "FILE" (T.pack $ show srcLocFile) cmd
      | otherwise = cmd <> " '" <> T.pack srcLocFile <> "'"

-- elisp = [i|(progn
--              (x-focus-frame (selected-frame))
--              (raise-frame)
--              (recenter)
--              )|]
