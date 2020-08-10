{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, FlexibleContexts, OverloadedStrings, NamedFieldPuns, ViewPatterns #-}
-- |

module Test.Sandwich.WebDriver.Video where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger hiding (logError)
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Default
import Data.Maybe
import Data.String.Interpolate.IsString
import System.Environment
import System.Exit
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows


#ifdef darwin_HOST_OS
import Safe
#endif

data VideoSettings = VideoSettings { x11grabOptions :: [String]
                                   -- ^ Arguments to x11grab, used with Linux.
                                   , avfoundationOptions :: [String]
                                   -- ^ Arguments to avfoundation, used with OS X.
                                   , gdigrabOptions :: [String]
                                   -- ^ Arguments to gdigrab, used with Windows.
                                   , hideMouseWhenRecording :: Bool
                                   -- ^ Hide the mouse while recording video. Linux and Windows only.
                                   }

fastX11VideoOptions = ["-an"
                      , "-r", "30"
                      , "-vcodec"
                      , "libxvid"
                      , "-qscale:v", "1"
                      , "-threads", "0"]

qualityX11VideoOptions = ["-an"
                         , "-r", "30"
                         , "-vcodec", "libx264"
                         , "-preset", "veryslow"
                         , "-crf", "0"
                         , "-threads", "0"]

defaultAvfoundationOptions = ["-r", "30"
                             , "-an"
                             , "-vcodec", "libxvid"
                             , "-qscale:v", "1"
                             , "-threads", "0"]

defaultGdigrabOptions = ["-framerate", "30"]

instance Default VideoSettings where
  def = VideoSettings { x11grabOptions = fastX11VideoOptions
                      , avfoundationOptions = defaultAvfoundationOptions
                      , gdigrabOptions = defaultGdigrabOptions
                      , hideMouseWhenRecording = False }

ffmpegOutfile :: String
ffmpegOutfile = "ffmpeg_stdout.txt"
ffmpegErrfile :: String
ffmpegErrfile = "ffmpeg_stderr.txt"

startFullScreenVideoRecording :: (MonadIO m, MonadReader context m, MonadLogger m, HasWebDriverContext context, MonadBaseControl IO m) =>
  FilePath -> VideoSettings -> m ProcessHandle
startFullScreenVideoRecording path videoSettings = do
  sess <- getContext webdriver
  let maybeXvfbSession = getXvfbSession sess
  (width, height) <- case maybeXvfbSession of
    Just (XvfbSession {xvfbDimensions}) -> return xvfbDimensions
    Nothing -> do
      (x, y, w, h) <- liftIO $ getScreenResolution sess
      return (fromIntegral w, fromIntegral h)
  startVideoRecording path (fromIntegral width, fromIntegral height, 0, 0) videoSettings maybeXvfbSession

startVideoRecording :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => FilePath -> (Word, Word, Int, Int) -> VideoSettings -> Maybe XvfbSession -> m ProcessHandle
startVideoRecording path (width, height, x, y) vs@(VideoSettings {..}) maybeXvfbSession = do
  cp <- liftIO $ getVideoArgs path (width, height, x, y) (VideoSettings {..}) maybeXvfbSession

  p <- createProcessWithLogging $ cp {
    create_group = True
    , std_in = NoStream
    }

  return p

endVideoRecording :: (MonadIO m, MonadLogger m, MonadCatch m) => ProcessHandle -> m ()
endVideoRecording p = do
  catchAny (liftIO $ interruptProcessGroupOf p)
           (\e -> logError [i|Exception in interruptProcessGroupOf in endVideoRecording: #{e}|])

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()
    ExitFailure n -> logError [i|ffmpeg exited with nonzero exit code #{n}'|]



getVideoArgs path (width, height, x, y) (VideoSettings {..}) maybeXvfbSession = do
#ifdef linux_HOST_OS
  displayNum <- case maybeXvfbSession of
    Nothing -> fromMaybe "" <$> (liftIO $ lookupEnv "DISPLAY")
    Just (XvfbSession {xvfbDisplayNum}) -> return $ ":" <> show xvfbDisplayNum

  let videoPath = [i|#{path}.avi|]
  let env' = [("DISPLAY", displayNum)]
  let env = case maybeXvfbSession of
       Nothing -> Just env'
       Just (XvfbSession {xvfbXauthority}) -> Just (("XAUTHORITY", xvfbXauthority) : env')
  let cmd = ["-draw_mouse", (if hideMouseWhenRecording then "0" else "1")
            , "-y"
            , "-nostdin"
            , "-f", "x11grab"
            , "-s", [i|#{width}x#{height}|]
            , "-i", [i|#{displayNum}.0+#{x},#{y}|]]
            ++ x11grabOptions
            ++ [videoPath]
  return ((proc "ffmpeg" cmd) { env = env })
#endif

#ifdef darwin_HOST_OS
  maybeScreenNumber <- liftIO getMacScreenNumber
  let videoPath = [i|#{path}.avi|]
  let cmd = case maybeScreenNumber of
    Just screenNumber -> ["-y"
                         , "-nostdin"
                         , "-f", "avfoundation"
                         , "-i", [i|#{screenNumber}|]]
                         ++ avfoundationOptions
                         ++ [videoPath]
    Nothing -> error [i|Not launching ffmpeg since OS X screen number couldn't be determined.|]
  return ((proc "ffmpeg" cmds) { env = Nothing })
#endif

#ifdef mingw32_HOST_OS
  let videoPath = [i|#{path}.mkv|]
  let cmd = ["-f", "gdigrab"
            , "-nostdin"
            , "-draw_mouse", (if hideMouseWhenRecording then "0" else "1")
            , "-i", "desktop"]
            ++ gdigrabOptions
            ++ [videoPath]
  return ((proc "ffmpeg.exe" cmd) { env = Nothing })
#endif
