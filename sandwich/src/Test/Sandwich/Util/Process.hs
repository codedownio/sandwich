{-# LANGUAGE CPP #-}

module Test.Sandwich.Util.Process (
  gracefullyStopProcess
  , gracefullyWaitForProcess

  , gracefullyStopProcess'
  , gracefullyWaitForProcess'

  , StopProcessResult(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import Data.Maybe
import Data.String.Interpolate
import System.Process
import Test.Sandwich.Logging

#ifndef mingw32_HOST_OS
import System.Posix.Signals (signalProcess, sigKILL)
#endif


data StopProcessResult =
  StoppedByItself
  | StoppedAfterInterrupt
  | StoppedAfterTerminate
  | StoppedAfterKill
  | FailedToStop
  deriving (Show, Eq, Ord, Enum)

-- | Interrupt a process and wait for it to terminate, returning a 'StopProcessResult'.
gracefullyStopProcess :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m ()
gracefullyStopProcess p gracePeriodUs = void $ gracefullyStopProcess' p gracePeriodUs

-- | Interrupt a process and wait for it to terminate.
gracefullyStopProcess' :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m StopProcessResult
gracefullyStopProcess' p gracePeriodUs = do
  liftIO $ interruptProcessGroupOf p
  gracefullyWaitForProcess' p gracePeriodUs

-- | Wait for a process to terminate. If it doesn't terminate within 'gracePeriodUs' microseconds,
-- send it an interrupt signal and wait for another 'gracePeriodUs' microseconds.
-- After this time elapses send a terminate signal and wait for the process to die.
gracefullyWaitForProcess :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m ()
gracefullyWaitForProcess p gracePeriodUs = void $ gracefullyWaitForProcess' p gracePeriodUs

-- | Wait for a process to terminate.
--
-- If it doesn't terminate within 'gracePeriodUs' microseconds, send it an
-- interrupt signal and wait for another 'gracePeriodUs' microseconds.
--
-- If it doesn't die by this time, send a terminate signal and wait again.
--
-- If it still isn't dead, send a kill signal.
gracefullyWaitForProcess' :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m StopProcessResult
gracefullyWaitForProcess' p gracePeriodUs = do
  let waitForExit = do
        let policy = limitRetriesByCumulativeDelay gracePeriodUs $ capDelay 200_000 $ exponentialBackoff 1_000
        retrying policy (\_ x -> return $ isNothing x) $ \_ -> do
          liftIO $ getProcessExitCode p

  waitForExit >>= \case
    Just _ -> return StoppedByItself
    Nothing -> do
      liftIO (getPid p) >>= \case
        Nothing -> return StoppedByItself
        Just pid -> do
          warn [i|(#{pid}) Process didn't stop after #{gracePeriodUs}us; trying to interrupt|]

          liftIO $ interruptProcessGroupOf p
          waitForExit >>= \case
            Just _ -> return StoppedAfterInterrupt
            Nothing -> do
              warn [i|(#{pid}) Process didn't stop after another sigINT and a further #{gracePeriodUs}us; going to terminate|]
              liftIO $ terminateProcess p

#ifdef mingw32_HOST_OS
              waitForExit >>= \case
                Just _ -> return StoppedAfterKill
                Nothing -> return FailedToStop
#else
              waitForExit >>= \case
                Just _ -> return StoppedAfterTerminate
                Nothing -> do
                  warn [i|(#{pid}) Process didn't stop after sigTERM and a further #{gracePeriodUs}us; going to kill|]
                  liftIO $ signalProcess sigKILL pid
                  waitForExit >>= \case
                    Just _ -> return StoppedAfterKill
                    Nothing -> return FailedToStop
#endif
