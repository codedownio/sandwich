{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

-- | Functions for recording videos of browser windows.

module Test.Sandwich.WebDriver.Video (
  startBrowserVideoRecording
  , startFullScreenVideoRecording

  -- * Lower-level
  , startVideoRecording
  , endVideoRecording

  -- * Wrap a test to conditionally record video
  , recordVideoIfConfigured

  -- * Configuration
  , VideoSettings(..)
  , defaultVideoSettings
  , fastX11VideoOptions
  , qualityX11VideoOptions
  , defaultAvfoundationOptions
  , defaultGdigrabOptions

  -- * Types
  , VideoProcess
  , videoProcessProcess
  , BaseVideoConstraints
  ) where

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger hiding (logError)
import Control.Monad.Reader (MonadReader)
import Data.Function (fix)
import Data.String.Interpolate
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import Test.Sandwich.WebDriver.Video.Internal
import Test.Sandwich.WebDriver.Video.Types
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver
import UnliftIO.Directory
import UnliftIO.Exception


type BaseVideoConstraints context m = (
  MonadLoggerIO m, MonadUnliftIO m, MonadMask m
  , MonadReader context m, HasBaseContext context, HasTestWebDriverContext context
  )

-- | A type representing a live video recording process
data VideoProcess = VideoProcess {
  -- | The process handle
  videoProcessProcess :: ProcessHandle
  , videoProcessCreatedFiles :: [FilePath]
  }
-- defaultVideoProcess :: ProcessHandle -> VideoProcess
-- defaultVideoProcess p = VideoProcess p mempty

-- | Wrapper around 'startVideoRecording' which uses the full screen dimensions.
startFullScreenVideoRecording :: (
  BaseVideoConstraints context m
  )
  -- | Output path
  => FilePath
  -> VideoSettings
  -> m VideoProcess
startFullScreenVideoRecording path videoSettings = do
  sess <- getContext webdriver
  -- let maybeXvfbSession = getXvfbSession sess
  let maybeXvfbSession = Nothing
  (width, height) <- case maybeXvfbSession of
    Just (XvfbSession {xvfbDimensions}) -> return xvfbDimensions
    Nothing -> do
      (_x, _y, w, h) <- getScreenResolution sess
      return (fromIntegral w, fromIntegral h)
  startVideoRecording path (fromIntegral width, fromIntegral height, 0, 0) videoSettings

-- | Wrapper around 'startVideoRecording' which uses WebDriver to find the rectangle corresponding to the browser.
startBrowserVideoRecording :: (
  BaseVideoConstraints context m, WebDriver m
  )
  -- | Output path
  => FilePath
  -> VideoSettings
  -> m VideoProcess
startBrowserVideoRecording path videoSettings = do
  Rect x y w h <- getWindowRect
  startVideoRecording path (w, h, x, y) videoSettings

-- | Record video to a given path, for a given screen rectangle.
startVideoRecording :: (
  BaseVideoConstraints context m
  )
  -- | Output path
  => FilePath
  -- | Rectangle to record, specified as @(width, height, x, y)@
  -> (Float, Float, Float, Float)
  -> VideoSettings
  -- | Returns handle to video process and list of files created
  -> m VideoProcess
startVideoRecording path (width, height, x, y) vs = do
  sess <- getContext webdriver
  -- let maybeXvfbSession = getXvfbSession sess
  let maybeXvfbSession = Nothing

  (cp', videoPath) <- getVideoArgs path (width, height, x, y) vs maybeXvfbSession
  let cp = cp' { create_group = True }

  case cmdspec cp of
    ShellCommand s -> debug [i|ffmpeg command: #{s}|]
    RawCommand p args -> debug [i|ffmpeg command: #{p} #{unwords args}|]

  case logToDisk vs of
    False -> do
      p <- createProcessWithLogging cp
      return $ VideoProcess p [videoPath]
    True -> do
      let stdoutPath = path <.> "stdout" <.> "log"
      let stderrPath = path <.> "stderr" <.> "log"
      liftIO $ bracket (openFile stdoutPath AppendMode) hClose $ \hout ->
        bracket (openFile stderrPath AppendMode) hClose $ \herr -> do
          (_, _, _, p) <- createProcess (cp { std_out = UseHandle hout, std_err = UseHandle herr })
          return $ VideoProcess p [videoPath, stdoutPath, stderrPath]

-- | Gracefully stop the 'ProcessHandle' returned by 'startVideoRecording'.
endVideoRecording :: (
  MonadLoggerIO m, MonadUnliftIO m
  ) => VideoProcess -> m ()
endVideoRecording (VideoProcess { videoProcessProcess=p }) = do
  catchAny (liftIO $ interruptProcessGroupOf p)
           (\e -> logError [i|Exception in interruptProcessGroupOf in endVideoRecording: #{e}|])

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()

    -- ffmpeg seems to exit with code 255 when exiting in response to a signal
    -- https://github.com/FFmpeg/FFmpeg/blob/d182d8f10cf69c59ef9c21df4b06e5478df063ef/fftools/ffmpeg.c#L4890
    ExitFailure 255 -> return ()

    ExitFailure n -> debug [i|ffmpeg exited with unexpected exit code #{n}'|]

-- * Wrappers

-- | Record video around a given action, if configured to do so in the 'CommandLineWebdriverOptions'.
--
-- This can be used to record video around individual tests. It can also keep videos only in case of
-- exceptions, deleting them on successful runs.
recordVideoIfConfigured :: (
  BaseVideoConstraints context m, WebDriver m, HasSomeCommandLineOptions context
  )
  -- | Session name
  => String
  -> m a
  -> m a
recordVideoIfConfigured browser action = do
  getCurrentFolder >>= \case
    Nothing -> action
    Just folder -> do
      SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})}) <- getSomeCommandLineOptions
      if | optIndividualVideos -> withVideo folder browser action
         | optErrorVideos -> withVideoIfException folder browser action
         | otherwise -> action

withVideo :: (
  BaseVideoConstraints context m, WebDriver m
  ) => FilePath -> String -> m a -> m a
withVideo folder browser action = do
  path <- getPathInFolder folder browser
  bracket (startBrowserVideoRecording path defaultVideoSettings) endVideoRecording (const action)

withVideoIfException :: (
  BaseVideoConstraints context m, WebDriver m
  ) => FilePath -> String -> m a -> m a
withVideoIfException folder browser action = do
  path <- getPathInFolder folder browser
  tryAny (bracket (startBrowserVideoRecording path defaultVideoSettings)
                  endVideoRecording
                  (\(VideoProcess {..}) -> (videoProcessCreatedFiles, ) <$> action))
    >>= \case
      Right (pathsToRemove, ret) -> do
        info [i|pathsToRemove: #{pathsToRemove}|]
        forM_ pathsToRemove removePathForcibly
        return ret
      Left e -> throwIO e

getPathInFolder :: (MonadUnliftIO m) => [Char] -> String -> m FilePath
getPathInFolder folder browser = flip fix (0 :: Integer) $ \loop n -> do
  let path = folder </> [i|#{browser}_video_#{n}|]
  liftIO (doesFileExist (path <> videoExtension)) >>= \case
    False -> return path
    True -> loop (n + 1)
