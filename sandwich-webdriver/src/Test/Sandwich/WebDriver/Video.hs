{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Functions for recording videos of browser windows.

module Test.Sandwich.WebDriver.Video (
  startBrowserVideoRecording
  , startFullScreenVideoRecording

  -- * Lower-level
  , startVideoRecording
  , endVideoRecording

  -- * Wrap an ExampleT to conditionally record video
  , recordVideoIfConfigured

  -- * Configuration
  , VideoSettings(..)
  , defaultVideoSettings
  , fastX11VideoOptions
  , qualityX11VideoOptions
  , defaultAvfoundationOptions
  , defaultGdigrabOptions

  -- * Types
  , BaseVideoConstraints
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger hiding (logError)
import Control.Monad.Reader
import Data.Function
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
import Test.WebDriver.Class as W
import Test.WebDriver.Commands
import UnliftIO.Directory
import UnliftIO.Exception


type BaseVideoConstraints context m = (
  MonadLoggerIO m, MonadUnliftIO m, MonadMask m
  , MonadReader context m, HasBaseContext context, HasWebDriverContext context
  )

-- | Wrapper around 'startVideoRecording' which uses the full screen dimensions.
startFullScreenVideoRecording :: (
  BaseVideoConstraints context m
  )
  -- | Output path
  => FilePath
  -> VideoSettings
  -> m ProcessHandle
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
  )
  -- | Output path
  => FilePath
  -> VideoSettings
  -> m ProcessHandle
startBrowserVideoRecording path videoSettings = do
  (x, y) <- getWindowPos
  (w, h) <- getWindowSize
  startVideoRecording path (w, h, x, y) videoSettings

-- | Record video to a given path, for a given screen rectangle.
startVideoRecording :: (
  BaseVideoConstraints context m
  )
  -- | Output path
  => FilePath
  -- | Rectangle to record, specified as @(width, height, x, y)@
  -> (Word, Word, Int, Int)
  -> VideoSettings
  -- | Returns handle to video process
  -> m ProcessHandle
startVideoRecording path (width, height, x, y) vs = do
  sess <- getContext webdriver
  let maybeXvfbSession = getXvfbSession sess

  cp' <- getVideoArgs path (width, height, x, y) vs maybeXvfbSession
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

-- * Wrappers

recordVideoIfConfigured :: (
  BaseVideoConstraints context m, W.WebDriver m, HasSomeCommandLineOptions context
  ) => String -> m a -> m a
recordVideoIfConfigured browser action = do
  getCurrentFolder >>= \case
    Nothing -> action
    Just folder -> do
      SomeCommandLineOptions (CommandLineOptions {optWebdriverOptions=(CommandLineWebdriverOptions {..})}) <- getSomeCommandLineOptions
      if | optIndividualVideos -> withVideo folder browser action
         | optErrorVideos -> withVideoIfException folder browser action
         | otherwise -> action

withVideo :: (
  BaseVideoConstraints context m, W.WebDriver m
  ) => FilePath -> String -> m a -> m a
withVideo folder browser action = do
  path <- getPathInFolder folder browser
  bracket (startBrowserVideoRecording path defaultVideoSettings) endVideoRecording (const action)

withVideoIfException :: (
  BaseVideoConstraints context m, W.WebDriver m
  ) => FilePath -> String -> m a -> m a
withVideoIfException folder browser action = do
  path <- getPathInFolder folder browser
  tryAny (bracket (startBrowserVideoRecording path defaultVideoSettings) (endVideoRecording) (const action)) >>= \case
    Right ret -> removePathForcibly path >> return ret
    Left e -> throwIO e

getPathInFolder :: (MonadUnliftIO m) => [Char] -> String -> m FilePath
getPathInFolder folder browser = flip fix (0 :: Integer) $ \loop n -> do
  let path = folder </> [i|#{browser}_video_#{n}|]
  liftIO (doesFileExist (path <> videoExtension)) >>= \case
    False -> return path
    True -> loop (n + 1)
