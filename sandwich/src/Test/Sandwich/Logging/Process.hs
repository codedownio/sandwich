{-# LANGUAGE CPP #-}

-- | Functions for launching processes while capturing their output in the test logs.

module Test.Sandwich.Logging.Process (
  -- * Process functions with direct logging
  createProcessWithLogging
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
import Control.Monad.Reader
import Data.String.Interpolate
import Foreign.C.Error
import GHC.IO.Exception
import GHC.Stack
import System.IO
import System.IO.Error (mkIOError)
import Test.Sandwich.ManagedAsync
import Test.Sandwich.Types.RunTree
import UnliftIO.Async
import UnliftIO.Exception
import UnliftIO.Process

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif


-- | Spawn a process with its stdout and stderr connected to the logging system.
-- Every line output by the process will be fed to a 'debug' call.
createProcessWithLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => CreateProcess -> m (ProcessHandle, Async ())
createProcessWithLogging = withFrozenCallStack (createProcessWithLogging' LevelDebug)

-- | Spawn a process with its stdout and stderr connected to the logging system.
createProcessWithLogging' :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => LogLevel -> CreateProcess -> m (ProcessHandle, Async ())
createProcessWithLogging' logLevel cp = do
  runId <- baseContextRunId <$> asks getBaseContext
  (hRead, hWrite) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  streamsReaderAsy <- managedAsync runId "streams-reader" $ forever $ do
    line <- liftIO $ hGetLine hRead
    logOtherCS callStack logLevel [i|#{name}: #{line}|]

  (_, _, _, p) <- liftIO $ createProcess (cp { std_out = UseHandle hWrite, std_err = UseHandle hWrite })
  return (p, streamsReaderAsy)

-- | Like 'readCreateProcess', but capture the stderr output in the logs.
-- Every line output by the process will be fed to a 'debug' call.
readCreateProcessWithLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => CreateProcess -> String -> m String
readCreateProcessWithLogging = withFrozenCallStack (readCreateProcessWithLogging' LevelDebug)

-- | Like 'readCreateProcess', but capture the stderr output in the logs.
readCreateProcessWithLogging' :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => LogLevel -> CreateProcess -> String -> m String
readCreateProcessWithLogging' logLevel cp input = do
  runId <- baseContextRunId <$> asks getBaseContext
  (hReadErr, hWriteErr) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  let stderrReader = forever $ do
        line <- liftIO $ hGetLine hReadErr
        logOtherCS callStack logLevel [i|#{name}: #{line}|]

  (ex, output) <-
    managedWithAsync_ runId "stderr-reader" stderrReader $
    withCreateProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle hWriteErr }) $ \sin' sout _ p ->
      -- Do this just like 'readCreateProcess'
      -- https://hackage.haskell.org/package/process-1.6.17.0/docs/src/System.Process.html#readCreateProcess
      case (sin', sout) of
        (Just hIn, Just hOut) -> liftIO $ do
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
createProcessWithLoggingAndStdin :: (HasCallStack, MonadUnliftIO m, MonadFail m, MonadLogger m, HasBaseContextMonad context m) => CreateProcess -> String -> m (ProcessHandle, Async ())
createProcessWithLoggingAndStdin = withFrozenCallStack (createProcessWithLoggingAndStdin' LevelDebug)

-- | Spawn a process with its stdout and stderr connected to the logging system.
createProcessWithLoggingAndStdin' :: (HasCallStack, MonadUnliftIO m, MonadFail m, MonadLogger m, HasBaseContextMonad context m) => LogLevel -> CreateProcess -> String -> m (ProcessHandle, Async ())
createProcessWithLoggingAndStdin' logLevel cp input = do
  runId <- baseContextRunId <$> asks getBaseContext
  (hRead, hWrite) <- liftIO createPipe

  let name = case cmdspec cp of
        ShellCommand {} -> "shell"
        RawCommand path _ -> path

  readAsy <- managedAsync runId "read-process-streams" $ forever $ do
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

  return (p, readAsy)

-- | Higher level version of 'createProcessWithLogging', accepting a shell command.
callCommandWithLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => String -> m ()
callCommandWithLogging = withFrozenCallStack (callCommandWithLogging' LevelDebug)

-- | Higher level version of 'createProcessWithLogging'', accepting a shell command.
callCommandWithLogging' :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => LogLevel -> String -> m ()
callCommandWithLogging' logLevel cmd = do
  runId <- baseContextRunId <$> asks getBaseContext
  (hRead, hWrite) <- liftIO createPipe

  (_, _, _, p) <- liftIO $ createProcess (shell cmd) {
    delegate_ctlc = True
    , std_out = UseHandle hWrite
    , std_err = UseHandle hWrite
    }

  let streamsReader = forever $ do
        line <- liftIO $ hGetLine hRead
        logOtherCS callStack logLevel [i|#{cmd}: #{line}|]

  managedWithAsync_ runId "stderr-reader" streamsReader $
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
    let wait' = takeMVar waitVar >>= either throwIO return
    restore (body wait') `C.onException` killThread tid

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
