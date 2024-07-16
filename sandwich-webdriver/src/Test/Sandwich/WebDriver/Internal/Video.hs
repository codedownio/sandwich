{-# LANGUAGE CPP #-}

module Test.Sandwich.WebDriver.Internal.Video where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg
import Test.Sandwich.WebDriver.Internal.OnDemand
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Types.Video
import Test.Sandwich.WebDriver.Types
import UnliftIO.Environment

#ifdef darwin_HOST_OS
import Safe

getMacScreenNumber :: IO (Maybe Int)
getMacScreenNumber = undefined
#endif


getVideoArgs :: (
  MonadUnliftIO m, MonadLoggerIO m, MonadFail m
  , MonadReader context m, HasBaseContext context, HasWebDriverContext context
  ) => FilePath -> (Word, Word, Int, Int) -> VideoSettings -> Maybe XvfbSession -> m CreateProcess
getVideoArgs path (width, height, x, y) (VideoSettings {..}) maybeXvfbSession = do
  WebDriver {wdFfmpeg, wdFfmpegToUse} <- getContext webdriver
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

  let videoPath = [i|#{path}.avi|]

  let cmd = ["-y"
            , "-nostdin"
            , "-f", "x11grab"
            , "-s", [i|#{width}x#{height}|]
            , "-i", [i|#{displayNum}.0+#{x},#{y}|]]
            ++ xcbgrabOptions
            ++ [videoPath]
  return ((proc ffmpeg cmd) { env = Just env })
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
  return ((proc ffmpeg cmd) { env = Nothing })
#endif

#ifdef mingw32_HOST_OS
  let videoPath = [i|#{path}.mkv|]
  let cmd = ["-f", "gdigrab"
            , "-nostdin"
            , "-i", "desktop"]
            ++ gdigrabOptions
            ++ [videoPath]
  return ((proc ffmpeg cmd) { env = Nothing })
#endif
