{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Sandwich.Formatters.TerminalUI.CrossPlatform (
  openFileExplorerFolderPortable
  ) where

import Control.Monad
import System.Directory
import System.Process


-- | TODO: report exceptions here
openFileExplorerFolderPortable :: String -> IO ()
#ifdef mingw32_HOST_OS
openFileExplorerFolderPortable folder = do
  findExecutable "explorer.exe" >>= \case
    Just p -> void $ readCreateProcessWithExitCode (proc p [folder]) ""
    Nothing -> return ()
#else
openFileExplorerFolderPortable folder =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [folder]) ""
#endif
