{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}

-- | Logging functions.

module Test.Sandwich.Logging where

import Control.Concurrent.Async.Lifted
import qualified Control.Exception as C
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import Data.Text
import Foreign.C.Error
import GHC.IO.Exception
import GHC.Stack
import System.IO
import System.Process

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif


-- * Basic logging functions


-- | Log a message at level 'LevelDebug'.
debug :: (HasCallStack, MonadLogger m) => Text -> m ()
debug = logDebugCS callStack

-- | Log a message at level 'LevelInfo'.
info :: (HasCallStack, MonadLogger m) => Text -> m ()
info = logInfoCS callStack

-- | Log a message at level 'LevelWarn'.
warn :: (HasCallStack, MonadLogger m) => Text -> m ()
warn = logWarnCS callStack

-- | Log a message at level 'LevelError'.
logError :: (HasCallStack, MonadLogger m) => Text -> m ()
logError = logErrorCS callStack

-- | Log with a custom 'LogLevel'.
logOther :: (HasCallStack, MonadLogger m) => LogLevel -> Text -> m ()
logOther = logOtherCS callStack


-- * System.Process helpers
--
-- | Functions for launching processes while capturing their output in the logs.

-- | Spawn a process with its stdout and stderr connected to the logging system. Every line output by the process
-- will be fed to a 'debug' call.
createProcessWithLogging :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, HasCallStack) => CreateProcess -> m ProcessHandle
createProcessWithLogging cp = do
  (hRead, hWrite) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hRead
    debug [i|#{name}: #{line}|]

  (_, _, _, p) <- liftIO $ createProcess (cp { std_out = UseHandle hWrite, std_err = UseHandle hWrite })
  return p

-- | Spawn a process with its stdout and stderr connected to the logging system. Every line output by the process
-- will be fed to a 'debug' call.
createProcessWithLoggingAndStdin :: (MonadIO m, MonadFail m, MonadBaseControl IO m, MonadLogger m, HasCallStack) => CreateProcess -> String -> m ProcessHandle
createProcessWithLoggingAndStdin cp input = do
  (hRead, hWrite) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hRead
    debug [i|#{name}: #{line}|]

  (Just inh, _, _, p) <- liftIO $ createProcess (
    cp { std_out = UseHandle hWrite
       , std_err = UseHandle hWrite
       , std_in = CreatePipe }
    )

  unless (Prelude.null input) $
    liftIO $ ignoreSigPipe $ hPutStr inh input
  -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
  liftIO $ ignoreSigPipe $ hClose inh

  return p

  where
    -- Copied from System.Process
    ignoreSigPipe :: IO () -> IO ()
    ignoreSigPipe = C.handle $ \case
      IOError { ioe_type  = ResourceVanished, ioe_errno = Just ioe } | Errno ioe == ePIPE -> return ()
      e -> throwIO e


-- | Higher level version of 'createProcessWithLogging', accepting a shell command.
callCommandWithLogging :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => String -> m ()
callCommandWithLogging cmd = do
  (hRead, hWrite) <- liftIO createPipe

  (_, _, _, p) <- liftIO $ createProcess (shell cmd) {
    delegate_ctlc = True
    , std_out = UseHandle hWrite
    , std_err = UseHandle hWrite
    }

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hRead
    debug [i|#{cmd}: #{line}|]

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()
    ExitFailure r -> liftIO $ throw $ userError [i|callCommandWithLogging failed for '#{cmd}': '#{r}'|]
