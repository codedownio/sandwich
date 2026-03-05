{-# LANGUAGE CPP #-}

-- | Functions for launching processes while capturing their output to files in the test tree.

module Test.Sandwich.Logging.ProcessFileLogging (
  createProcessWithFileLogging
  , readCreateProcessWithFileLogging
  , createProcessWithFileLoggingAndStdin
  , callCommandWithFileLogging
  ) where

import Control.Concurrent
import Control.DeepSeq (rnf)
import qualified Control.Exception as C
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Foreign.C.Error
import GHC.IO.Exception
import GHC.Stack
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich.Contexts
import Test.Sandwich.Expectations
import Test.Sandwich.Types.RunTree
import UnliftIO.Exception

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif


-- | Derive a short name from a 'CreateProcess' for use in log file names.
-- For 'RawCommand', takes the base filename of the executable.
-- For 'ShellCommand', takes the first word of the command string.
processName :: CreateProcess -> String
processName cp = case cmdspec cp of
  RawCommand path _ -> takeFileName path
  ShellCommand cmd -> case words cmd of
    (w:_) -> takeFileName w
    [] -> "shell"

-- | Spawn a process with its stdout and stderr logged to files in the test tree.
createProcessWithFileLogging :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  ) => CreateProcess -> m ProcessHandle
createProcessWithFileLogging cp = withFrozenCallStack $ do
  let name = processName cp
  getCurrentFolder >>= \case
    Nothing -> expectationFailure [i|createProcessWithFileLogging: no current folder, so unable to log for '#{name}'.|]
    Just dir ->
      bracket (liftIO $ openTempFile dir (name <.> "out")) (\(_outfile, hOut) -> liftIO $ hClose hOut) $ \(_outfile, hOut) ->
      bracket (liftIO $ openTempFile dir (name <.> "err")) (\(_errfile, hErr) -> liftIO $ hClose hErr) $ \(_errfile, hErr) -> do
        (_, _, _, p) <- liftIO $ createProcess (cp { std_out = UseHandle hOut, std_err = UseHandle hErr })
        return p

-- | Like 'readCreateProcess', but capture the stderr output to a file in the test tree.
-- Returns the stdout output as a 'String'.
readCreateProcessWithFileLogging :: (
  HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m
  ) => CreateProcess -> String -> m String
readCreateProcessWithFileLogging cp input = withFrozenCallStack $ do
  let name = processName cp
  getCurrentFolder >>= \case
    Nothing -> expectationFailure [i|readCreateProcessWithFileLogging: no current folder, so unable to log for '#{name}'.|]
    Just dir ->
      bracket (liftIO $ openTempFile dir (name <.> "err")) (\(_errfile, hErr) -> liftIO $ hClose hErr) $ \(_errfile, hErr) -> do
        (ex, output) <- liftIO $ withCreateProcess (cp { std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle hErr }) $ \sin' sout _ p -> do
          case (sin', sout) of
            (Just hIn, Just hOut) -> do
              output <- hGetContents hOut
              withForkWait (C.evaluate $ rnf output) $ \waitOut -> do
                unless (Prelude.null input) $
                  ignoreSigPipe $ hPutStr hIn input
                ignoreSigPipe $ hClose hIn
                waitOut
                hClose hOut
              ex <- waitForProcess p
              return (ex, output)
            (Nothing, _) -> throwIO $ userError "readCreateProcessWithFileLogging: Failed to get a stdin handle."
            (_, Nothing) -> throwIO $ userError "readCreateProcessWithFileLogging: Failed to get a stdout handle."
        case ex of
          ExitSuccess -> return output
          ExitFailure r -> liftIO $ throwIO $ userError [i|readCreateProcessWithFileLogging failed for '#{name}': exit code #{r}|]

-- | Spawn a process with its stdout and stderr logged to files in the test tree,
-- passing the given string as stdin.
createProcessWithFileLoggingAndStdin :: (HasCallStack, MonadUnliftIO m, MonadFail m, MonadLogger m, HasBaseContextMonad context m) => CreateProcess -> String -> m ProcessHandle
createProcessWithFileLoggingAndStdin cp input = withFrozenCallStack $ do
  let name = processName cp
  getCurrentFolder >>= \case
    Nothing -> expectationFailure [i|createProcessWithFileLoggingAndStdin: no current folder, so unable to log for '#{name}'.|]
    Just dir ->
      bracket (liftIO $ openTempFile dir (name <.> "out")) (\(_outfile, hOut) -> liftIO $ hClose hOut) $ \(_outfile, hOut) ->
      bracket (liftIO $ openTempFile dir (name <.> "err")) (\(_errfile, hErr) -> liftIO $ hClose hErr) $ \(_errfile, hErr) -> do
        (Just inh, _, _, p) <- liftIO $ createProcess (cp { std_out = UseHandle hOut
                                                           , std_err = UseHandle hErr
                                                           , std_in = CreatePipe })
        unless (Prelude.null input) $
          liftIO $ ignoreSigPipe $ hPutStr inh input
        liftIO $ ignoreSigPipe $ hClose inh
        return p

-- | Higher level version of 'createProcessWithFileLogging', accepting a shell command.
callCommandWithFileLogging :: (HasCallStack, MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => String -> m ()
callCommandWithFileLogging cmd = withFrozenCallStack $ do
  let name = processName (shell cmd)
  getCurrentFolder >>= \case
    Nothing -> expectationFailure [i|callCommandWithFileLogging: no current folder, so unable to log for '#{name}'.|]
    Just dir ->
      bracket (liftIO $ openTempFile dir (name <.> "out")) (\(_outfile, hOut) -> liftIO $ hClose hOut) $ \(_outfile, hOut) ->
      bracket (liftIO $ openTempFile dir (name <.> "err")) (\(_errfile, hErr) -> liftIO $ hClose hErr) $ \(_errfile, hErr) -> do
        (_, _, _, p) <- liftIO $ createProcess (shell cmd) {
          delegate_ctlc = True
          , std_out = UseHandle hOut
          , std_err = UseHandle hErr
          }
        liftIO (waitForProcess p) >>= \case
          ExitSuccess -> return ()
          ExitFailure r -> liftIO $ throwIO $ userError [i|callCommandWithFileLogging failed for '#{cmd}': '#{r}'|]


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
