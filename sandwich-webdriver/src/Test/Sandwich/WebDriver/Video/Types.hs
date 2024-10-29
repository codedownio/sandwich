
module Test.Sandwich.WebDriver.Video.Types where


-- | Default options for fast X11 video recording.
fastX11VideoOptions :: [String]
fastX11VideoOptions = ["-an"
                      , "-r", "30"
                      , "-vcodec"
                      , "libxvid"
                      , "-qscale:v", "1"
                      , "-threads", "0"]

-- | Default options for quality X11 video recording.
qualityX11VideoOptions :: [String]
qualityX11VideoOptions = ["-an"
                         , "-r", "30"
                         , "-vcodec", "libx264"
                         , "-preset", "veryslow"
                         , "-crf", "0"
                         , "-threads", "0"]

-- | Default options for AVFoundation recording (for Darwin).
defaultAvfoundationOptions :: [String]
defaultAvfoundationOptions = ["-r", "30"
                             , "-an"
                             , "-vcodec", "libxvid"
                             , "-qscale:v", "1"
                             , "-threads", "0"]

-- | Default options for gdigrab recording (for Windows).
defaultGdigrabOptions :: [String]
defaultGdigrabOptions = ["-framerate", "30"]

data VideoSettings = VideoSettings {
  xcbgrabOptions :: [String]
  -- ^ Arguments to x11grab, used with Linux.
  , avfoundationOptions :: [String]
  -- ^ Arguments to avfoundation, used with OS X.
  , gdigrabOptions :: [String]
  -- ^ Arguments to gdigrab, used with Windows.
  , hideMouseWhenRecording :: Bool
  -- ^ Hide the mouse while recording video. Linux and Windows only.
  , logToDisk :: Bool
  -- ^ Log ffmpeg stdout and stderr to disk.
  }

-- | Default video settings.
defaultVideoSettings :: VideoSettings
defaultVideoSettings = VideoSettings {
  xcbgrabOptions = fastX11VideoOptions
  , avfoundationOptions = defaultAvfoundationOptions
  , gdigrabOptions = defaultGdigrabOptions
  , hideMouseWhenRecording = False
  , logToDisk = True
  }
