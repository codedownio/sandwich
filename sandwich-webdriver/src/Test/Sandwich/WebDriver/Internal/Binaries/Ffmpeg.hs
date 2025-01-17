{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg (
  obtainFfmpeg

  -- * Types
  , FfmpegToUse(..)
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg.Types
import UnliftIO.Directory


-- | Manually obtain an ffmpeg binary, according to the 'FfmpegToUse' policy.
obtainFfmpeg :: (
  MonadReader context m, HasBaseContext context
  , MonadUnliftIO m, MonadLoggerIO m, MonadMask m
  ) => FfmpegToUse -> m (Either T.Text FilePath)
obtainFfmpeg UseFfmpegFromPath = findExecutable "ffmpeg" >>= \case
  Nothing -> return $ Left [i|Couldn't find "ffmpeg" on the PATH.|]
  Just p -> do
    debug [i|Found ffmpeg at #{p}|]
    return $ Right p
obtainFfmpeg (UseFfmpegAt path) = doesFileExist path >>= \case
  False -> return $ Left [i|Path '#{path}' didn't exist|]
  True -> do
    debug [i|Found ffmpeg at #{path}|]
    return $ Right path
obtainFfmpeg (UseFfmpegFromNixpkgs nixContext) = do
  debug [i|Building ffmpeg with Nix...|]
  ret <- getBinaryViaNixDerivation' @"ffmpeg" nixContext ffmpegDerivation
  debug [i|Built ffmpeg with Nix: #{ret}|]
  return $ Right ret

ffmpegDerivation :: T.Text
ffmpegDerivation = [i|
{ ffmpeg
}:

ffmpeg.override { withXcb = true; }
|]
