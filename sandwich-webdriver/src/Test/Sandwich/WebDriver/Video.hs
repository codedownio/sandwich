{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.Sandwich.WebDriver.Video (
  startVideoRecording
  , endVideoRecording

  -- * Helpers
  , startFullScreenVideoRecording
  , startBrowserVideoRecording

  -- * Configuration
  , VideoSettings(..)
  , defaultVideoSettings
  , fastX11VideoOptions
  , qualityX11VideoOptions
  , defaultAvfoundationOptions
  , defaultGdigrabOptions
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger hiding (logError)
import Control.Monad.Reader
import Data.String.Interpolate
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Video
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver.Class as W
import Test.WebDriver.Commands
import UnliftIO.Exception


type BaseVideoConstraints context m = (MonadLoggerIO m, MonadUnliftIO m, MonadReader context m, HasWebDriverContext context)

-- | Wrapper around 'startVideoRecording' which uses the full screen dimensions.
startFullScreenVideoRecording :: (
  BaseVideoConstraints context m
  ) => FilePath -> VideoSettings -> m ProcessHandle
startFullScreenVideoRecording path videoSettings = do
  sess <- getContext webdriver
  let maybeXvfbSession = getXvfbSession sess
  (width, height) <- case maybeXvfbSession of
    Just (XvfbSession {xvfbDimensions}) -> return xvfbDimensions
    Nothing -> do
      (_x, _y, w, h) <- getScreenResolution sess
      return (fromIntegral w, fromIntegral h)
  startVideoRecording path (fromIntegral width, fromIntegral height, 0, 0) videoSettings

-- | Wrapper around 'startVideoRecording' which uses WebDriver to find the rectangle corresponding to the browser.
startBrowserVideoRecording :: (
  BaseVideoConstraints context m, W.WebDriver m
  ) => FilePath -> VideoSettings -> m ProcessHandle
startBrowserVideoRecording path videoSettings = do
  (x, y) <- getWindowPos
  (w, h) <- getWindowSize
  startVideoRecording path (w, h, x, y) videoSettings

-- | Record video to a given path, for a given rectangle specified as (width, height, x, y).
startVideoRecording :: (
  BaseVideoConstraints context m
  ) => FilePath -> (Word, Word, Int, Int) -> VideoSettings -> m ProcessHandle
startVideoRecording path (width, height, x, y) vs = do
  sess <- getContext webdriver
  let maybeXvfbSession = getXvfbSession sess

  cp' <- liftIO $ getVideoArgs path (width, height, x, y) vs maybeXvfbSession
  let cp = cp' { create_group = True }

  case cmdspec cp of
    ShellCommand s -> debug [i|ffmpeg command: #{s}|]
    RawCommand p args -> debug [i|ffmpeg command: #{p} #{unwords args}|]

  case logToDisk vs of
    False -> createProcessWithLogging cp
    True -> do
      liftIO $ bracket (openFile (path <.> "stdout" <.> "log") AppendMode) hClose $ \hout ->
        bracket (openFile (path <.> "stderr" <.> "log") AppendMode) hClose $ \herr -> do
          (_, _, _, p) <- createProcess (cp { std_out = UseHandle hout, std_err = UseHandle herr })
          return p

-- | Gracefully stop the 'ProcessHandle' returned by 'startVideoRecording'.
endVideoRecording :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => ProcessHandle -> m ()
endVideoRecording p = do
  catchAny (liftIO $ interruptProcessGroupOf p)
           (\e -> logError [i|Exception in interruptProcessGroupOf in endVideoRecording: #{e}|])

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()

    -- ffmpeg seems to exit with code 255 when exiting in response to a signal
    -- https://github.com/FFmpeg/FFmpeg/blob/d182d8f10cf69c59ef9c21df4b06e5478df063ef/fftools/ffmpeg.c#L4890
    ExitFailure 255 -> return ()

    ExitFailure n -> debug [i|ffmpeg exited with unexpected exit code #{n}'|]
