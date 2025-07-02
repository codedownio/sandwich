{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE CPP #-}

module Test.Sandwich.WebDriver.Video.Internal (
  getVideoArgs
  , videoExtension
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.OnDemand
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Types
import Test.Sandwich.WebDriver.Video.Types

#ifdef darwin_HOST_OS
getMacScreenNumber :: IO (Maybe Int)
getMacScreenNumber = return $ Just 0 -- TODO
#endif

#ifdef linux_HOST_OS
import Data.Function ((&), on)
import qualified Data.List as L
import Data.Maybe
import System.FilePath ((<.>))
import UnliftIO.Environment
#endif


videoExtension :: String
videoExtension = "avi"

getVideoArgs :: (
  MonadUnliftIO m, MonadLoggerIO m
  , MonadReader context m, HasBaseContext context, HasWebDriverContext context
  ) => FilePath -> (Float, Float, Float, Float) -> VideoSettings -> Maybe XvfbSession -> m (CreateProcess, FilePath)
getVideoArgs path (width, height, x, y) (VideoSettings {..}) maybeXvfbSession = do
  WebDriverContext {wdFfmpeg, wdFfmpegToUse} <- getContext webdriver
  ffmpeg <- getOnDemand wdFfmpeg (obtainFfmpeg wdFfmpegToUse)

#ifdef linux_HOST_OS
  displayNum <- case maybeXvfbSession of
    Nothing -> fromMaybe "" <$> (liftIO $ lookupEnv "DISPLAY")
    Just (XvfbSession {xvfbDisplayNum}) -> return $ ":" <> show xvfbDisplayNum

  baseEnv <- getEnvironment

  let env = case maybeXvfbSession of
        Nothing -> baseEnv
        Just (XvfbSession {..}) -> baseEnv
          & (("DISPLAY", displayNum) :)
          & (("XAUTHORITY", xvfbXauthority) :)
          & L.nubBy ((==) `on` fst)

  let videoPath = path <.> videoExtension

  let cmd = ["-y"
            , "-nostdin"
            , "-f", "x11grab"
            , "-s", [i|#{width}x#{height}|]
            , "-i", [i|#{displayNum}.0+#{x},#{y}|]]
            ++ xcbgrabOptions
            ++ [videoPath]
  return ((proc ffmpeg cmd) { env = Just env }, videoPath)
#endif

#ifdef darwin_HOST_OS
  maybeScreenNumber <- liftIO getMacScreenNumber
  let videoPath = [i|#{path}.avi|]
  let cmd = case maybeScreenNumber of
        Just screenNumber -> ["-y"
                             , "-nostdin"
                             , "-f", "avfoundation"
                             , "-video-size", [i|#{width}x#{height}|]
                             , "-vf", [i|crop=#{width}:#{height}:#{x}:#{y}|]
                             , "-i", [i|#{screenNumber}|]]
                             ++ avfoundationOptions
                             ++ [videoPath]
        Nothing -> error [i|Not launching ffmpeg since OS X screen number couldn't be determined.|]
  return ((proc ffmpeg cmd) { env = Nothing }, videoPath)
#endif

#ifdef mingw32_HOST_OS
  let videoPath = [i|#{path}.mkv|]
  let cmd = ["-f", "gdigrab"
            , "-nostdin"
            , "-i", "desktop"
            , "-offset_x", [i|#{x}|]
            , "-offset_y", [i|#{y}|]
            , "-video-size", [i|#{width}x#{height}|]
            ]
            ++ gdigrabOptions
            ++ [videoPath]
  return ((proc ffmpeg cmd) { env = Nothing }, videoPath)
#endif
