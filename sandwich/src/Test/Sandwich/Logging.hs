{-# LANGUAGE CPP #-}

-- | Logging functions.

module Test.Sandwich.Logging (
  debug
  , info
  , warn
  , Test.Sandwich.Logging.logError
  , Test.Sandwich.Logging.logOther

  -- * Process functions with logging
  , createProcessWithLogging
  , readCreateProcessWithLogging
  , createProcessWithLoggingAndStdin
  , callCommandWithLogging

  , createProcessWithLogging'
  , readCreateProcessWithLogging'
  , createProcessWithLoggingAndStdin'
  , callCommandWithLogging'
  ) where

import Control.Concurrent
import Control.DeepSeq (rnf)
import qualified Control.Exception as C
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger hiding (logOther)
import Data.String.Interpolate
import Data.Text
import Foreign.C.Error
import GHC.IO.Exception
import GHC.Stack
import System.IO
import System.IO.Error (mkIOError)
import System.Process
import UnliftIO.Async hiding (wait)
import UnliftIO.Exception

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

-- | Spawn a process with its stdout and stderr connected to the logging system.
-- Every line output by the process will be fed to a 'debug' call.
createProcessWithLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => CreateProcess -> m ProcessHandle
createProcessWithLogging = withFrozenCallStack (createProcessWithLogging' LevelDebug)

-- | Spawn a process with its stdout and stderr connected to the logging system.
createProcessWithLogging' :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => LogLevel -> CreateProcess -> m ProcessHandle
createProcessWithLogging' logLevel cp = do
  (hRead, hWrite) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hRead
    logOtherCS callStack logLevel [i|#{name}: #{line}|]

  (_, _, _, p) <- liftIO $ createProcess (cp { std_out = UseHandle hWrite, std_err = UseHandle hWrite })
  return p

-- | Like 'readCreateProcess', but capture the stderr output in the logs.
-- Every line output by the process will be fed to a 'debug' call.
readCreateProcessWithLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => CreateProcess -> String -> m String
readCreateProcessWithLogging = withFrozenCallStack (readCreateProcessWithLogging' LevelDebug)

-- | Like 'readCreateProcess', but capture the stderr output in the logs.
readCreateProcessWithLogging' :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => LogLevel -> CreateProcess -> String -> m String
readCreateProcessWithLogging' logLevel cp input = do
  (hReadErr, hWriteErr) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hReadErr
    logOtherCS callStack logLevel [i|#{name}: #{line}|]

  -- Do this just like 'readCreateProcess'
  -- https://hackage.haskell.org/package/process-1.6.17.0/docs/src/System.Process.html#readCreateProcess
  (ex, output) <- liftIO $ withCreateProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle hWriteErr }) $ \sin' sout _ p -> do
    case (sin', sout) of
      (Just hIn, Just hOut) -> do
        output  <- hGetContents hOut
        withForkWait (C.evaluate $ rnf output) $ \waitOut -> do
          -- now write any input
          unless (Prelude.null input) $
            ignoreSigPipe $ hPutStr hIn input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose hIn

          -- wait on the output
          waitOut
          hClose hOut

        -- wait on the process
        ex <- waitForProcess p
        return (ex, output)
      (Nothing, _) -> liftIO $ throwIO $ userError "readCreateProcessWithStderrLogging: Failed to get a stdin handle."
      (_, Nothing) -> liftIO $ throwIO $ userError "readCreateProcessWithStderrLogging: Failed to get a stdout handle."

  case ex of
    ExitSuccess -> return output
    ExitFailure r -> liftIO $ processFailedException "readCreateProcessWithLogging" cmd args r

  where
    cmd = case cp of
            CreateProcess { cmdspec = ShellCommand sc } -> sc
            CreateProcess { cmdspec = RawCommand fp _ } -> fp
    args = case cp of
             CreateProcess { cmdspec = ShellCommand _ } -> []
             CreateProcess { cmdspec = RawCommand _ args' } -> args'


-- | Spawn a process with its stdout and stderr connected to the logging system.
-- Every line output by the process will be fed to a 'debug' call.
createProcessWithLoggingAndStdin :: (HasCallStack, MonadUnliftIO m, MonadFail m, MonadLogger m) => CreateProcess -> String -> m ProcessHandle
createProcessWithLoggingAndStdin = withFrozenCallStack (createProcessWithLoggingAndStdin' LevelDebug)

-- | Spawn a process with its stdout and stderr connected to the logging system.
createProcessWithLoggingAndStdin' :: (HasCallStack, MonadUnliftIO m, MonadFail m, MonadLogger m) => LogLevel -> CreateProcess -> String -> m ProcessHandle
createProcessWithLoggingAndStdin' logLevel cp input = do
  (hRead, hWrite) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hRead
    logOtherCS callStack logLevel [i|#{name}: #{line}|]

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

-- | Higher level version of 'createProcessWithLogging', accepting a shell command.
callCommandWithLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => String -> m ()
callCommandWithLogging = withFrozenCallStack (callCommandWithLogging' LevelDebug)

-- | Higher level version of 'createProcessWithLogging'', accepting a shell command.
callCommandWithLogging' :: (HasCallStack, MonadUnliftIO m, MonadLogger m) => LogLevel -> String -> m ()
callCommandWithLogging' logLevel cmd = do
  (hRead, hWrite) <- liftIO createPipe

  (_, _, _, p) <- liftIO $ createProcess (shell cmd) {
    delegate_ctlc = True
    , std_out = UseHandle hWrite
    , std_err = UseHandle hWrite
    }

  _ <- async $ forever $ do
    line <- liftIO $ hGetLine hRead
    logOtherCS callStack logLevel [i|#{cmd}: #{line}|]

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()
    ExitFailure r -> liftIO $ throwIO $ userError [i|callCommandWithLogging failed for '#{cmd}': '#{r}'|]


-- * Util

-- Copied from System.Process
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait asy body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore asy) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

-- Copied from System.Process
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \case
  IOError { ioe_type  = ResourceVanished, ioe_errno = Just ioe } | Errno ioe == ePIPE -> return ()
  e -> throwIO e

-- Copied from System.Process
processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     Prelude.concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)
