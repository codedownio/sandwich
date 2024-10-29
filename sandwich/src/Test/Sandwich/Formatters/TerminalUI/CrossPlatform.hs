{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Sandwich.Formatters.TerminalUI.CrossPlatform (
  openFileExplorerFolderPortable
  ) where

import Control.Monad
import System.Process


-- | TODO: report exceptions from this

#ifdef mingw32_HOST_OS
import System.Directory

openFileExplorerFolderPortable :: String -> IO ()
openFileExplorerFolderPortable folder = do
  findExecutable "explorer.exe" >>= \case
    Just p -> void $ readCreateProcessWithExitCode (proc p [folder]) ""
    Nothing -> return ()
#else
openFileExplorerFolderPortable :: String -> IO ()
openFileExplorerFolderPortable folder =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [folder]) ""
#endif
