{-# LANGUAGE CPP #-}

module Test.Sandwich.Formatters.TerminalUI.CrossPlatform (
  openFileExplorerFolderPortable
  , openUrlPortable
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

openUrlPortable :: String -> IO ()
openUrlPortable url = do
  findExecutable "explorer.exe" >>= \case
    Just p -> void $ readCreateProcessWithExitCode (proc p [url]) ""
    Nothing -> return ()
#elif darwin_HOST_OS
openFileExplorerFolderPortable :: String -> IO ()
openFileExplorerFolderPortable folder =
  void $ readCreateProcessWithExitCode (proc "open" [folder]) ""

openUrlPortable :: String -> IO ()
openUrlPortable url =
  void $ readCreateProcessWithExitCode (proc "open" [url]) ""
#else
openFileExplorerFolderPortable :: String -> IO ()
openFileExplorerFolderPortable folder =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [folder]) ""

openUrlPortable :: String -> IO ()
openUrlPortable url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""
#endif
