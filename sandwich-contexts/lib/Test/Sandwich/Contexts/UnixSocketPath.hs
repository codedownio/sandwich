{-# LANGUAGE CPP #-}

module Test.Sandwich.Contexts.UnixSocketPath (
  withUnixSocketDirectory
  , maxUnixSocketLength
  ) where

import Control.Monad.IO.Unlift
import Relude
import System.IO.Error (IOError)
import Test.Sandwich.Expectations (expectationFailure)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Temporary


-- | The longest allowed path for a Unix socket on the current system.
maxUnixSocketLength :: Int
#ifdef mingw32_HOST_OS
maxUnixSocketLength = Infinity
#elif darwin_host_os
maxUnixSocketLength = 103 -- macOS: 104 with null terminator
#else
maxUnixSocketLength = 107 -- Linux: 108 with null terminator
#endif

-- | Create a temporary directory in which a Unix socket can be safely created,
-- bearing in mind the longest allowed Unix socket path on the system.
withUnixSocketDirectory :: (MonadUnliftIO m)
  -- | Name template, as passed to 'withSystemTempDirectory'
  => String
  -- | Amount of headroom to leave for a file name in this directory,
  -- before hitting the 'maxUnixSocketLength'
  -> Int
  -- | Callback
  -> (FilePath -> m a) -> m a
withUnixSocketDirectory nameTemplate headroom action = do
  withSystemTempDirectory nameTemplate $ \dir ->
    if | length dir + headroom <= maxUnixSocketLength -> action dir
       | otherwise -> withShortTempDir nameTemplate headroom action

withShortTempDir :: (
  MonadUnliftIO m
  )
  => String
  -> Int
  -> (FilePath -> m a)
  -> m a
withShortTempDir nameTemplate headroom action = doesDirectoryExist "/tmp" >>= \case
  True -> isDirectoryWritable "/tmp" >>= \case
    True -> withTempDirectory "/tmp" nameTemplate $ \dir ->
      if | length dir + headroom <= maxUnixSocketLength -> action dir
         | otherwise -> doFail
    False -> doFail
  _ -> doFail
  where
    doFail = expectationFailure "Couldn't create a short enough Unix socket path on this system."

isDirectoryWritable :: MonadUnliftIO m => FilePath -> m Bool
isDirectoryWritable dir = do
  try (getPermissions dir) >>= \case
    Left (_ :: IOError) -> return False
    Right perms -> return $ writable perms
