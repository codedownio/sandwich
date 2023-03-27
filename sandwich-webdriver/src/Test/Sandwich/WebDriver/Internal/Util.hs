{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Sandwich.WebDriver.Internal.Util where

import Control.Exception
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Retry
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import System.Directory
import System.Process
import qualified System.Random as R
import Test.Sandwich.Logging

#ifdef mingw32_HOST_OS
import System.IO
#endif


-- * Truncating log files

moveAndTruncate :: FilePath -> String -> IO ()
moveAndTruncate from to = do
  exists <- doesFileExist from
  when exists $ do
    copyFile from to
    tryTruncateFile from

  where
    tryTruncateFile :: FilePath -> IO ()
    tryTruncateFile path = E.catch (truncateFile path)
                                   (\(e :: E.SomeException) -> putStrLn [i|Failed to truncate file #{path}: #{e}|])

    truncateFile :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
    truncateFile path = withFile path WriteMode $ flip hPutStr "\n" -- Not exactly truncation, but close enough?
#else
    truncateFile path = void $ readCreateProcess (shell [i|> #{path}|]) ""
#endif

-- * Exceptions

leftOnException :: (MonadIO m, MonadBaseControl IO m) => m (Either T.Text a) -> m (Either T.Text a)
leftOnException = E.handle (\(e :: SomeException) -> return $ Left $ T.pack $ show e)

leftOnException' :: (MonadIO m, MonadBaseControl IO m) => m a -> m (Either T.Text a)
leftOnException' action = E.catch (Right <$> action) (\(e :: SomeException) -> return $ Left $ T.pack $ show e)

-- * Util

whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = void $ action x

whenLeft :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) action = action x
whenLeft (Right _) _ = return ()

whenRight :: (Monad m) => Either a b -> (b -> m ()) -> m ()
whenRight (Left _) _ = return ()
whenRight (Right x) action = action x

makeUUID :: IO T.Text
makeUUID = (T.pack . take 10 . R.randomRs ('a','z')) <$> R.newStdGen

-- * Stopping processes

gracefullyStopProcess :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m ()
gracefullyStopProcess p gracePeriodUs = do
  liftIO $ interruptProcessGroupOf p
  gracefullyWaitForProcess p gracePeriodUs

gracefullyWaitForProcess :: (MonadIO m, MonadLogger m) => ProcessHandle -> Int -> m ()
gracefullyWaitForProcess p gracePeriodUs = do
  let waitForExit = do
        let policy = limitRetriesByCumulativeDelay gracePeriodUs $ capDelay 200_000 $ exponentialBackoff 1_000
        retrying policy (\_ x -> return $ isNothing x) $ \_ -> do
          liftIO $ getProcessExitCode p

  waitForExit >>= \case
    Just _ -> return ()
    Nothing -> do
      pid <- liftIO $ getPid p
      warn [i|(#{pid}) Process didn't stop after #{gracePeriodUs}us; trying to interrupt|]

      liftIO $ interruptProcessGroupOf p
      waitForExit >>= \case
        Just _ -> return ()
        Nothing -> void $ do
          warn [i|(#{pid}) Process didn't stop after a further #{gracePeriodUs}us; going to kill|]
          liftIO $ terminateProcess p
          liftIO $ waitForProcess p
