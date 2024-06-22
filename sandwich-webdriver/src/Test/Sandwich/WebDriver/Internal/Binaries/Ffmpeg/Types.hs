
module Test.Sandwich.WebDriver.Internal.Binaries.Ffmpeg.Types (
  FfmpegToUse(..)
  ) where

import Test.Sandwich.Contexts.Nix

-- | How to obtain the @ffmpeg@ binary.
data FfmpegToUse =
  -- | Search the PATH for the @ffmpeg@ binary.
  UseFfmpegFromPath
  -- | Use the @ffmpeg@ at the given path.
  | UseFfmpegAt FilePath
  -- | Get @ffmpeg@ from Nixpkgs.
  | UseFfmpegFromNixpkgs NixContext
  deriving Show
