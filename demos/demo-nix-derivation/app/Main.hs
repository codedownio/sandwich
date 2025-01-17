{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.String.Interpolate
import Data.Text
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import UnliftIO.Process


spec :: TopSpec
spec = describe "Introducing a Nix derivation" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceFileViaNixDerivation @"ffmpeg" ffmpegDerivation $ do
      it "uses the ffmpeg binary" $ do
        useFfmpeg

useFfmpeg :: (MonadIO m, MonadReader context m, MonadLogger m, HasFile context "ffmpeg") => m ()
useFfmpeg = do
  ffmpeg <- askFile @"ffmpeg"
  output <- readCreateProcess (proc ffmpeg ["-version"]) ""
  info [i|Ffmpeg version output: #{output}|]

-- | This demonstrates building an arbitrary callPackage-style Nix derivation. Here, we
-- take in @ffmpeg@ and change one of its settings.
ffmpegDerivation :: Text
ffmpegDerivation = [i|
{ ffmpeg
}:

ffmpeg.override { withXcb = true; }
|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
