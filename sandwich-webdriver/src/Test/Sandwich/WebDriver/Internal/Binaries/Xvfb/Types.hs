
module Test.Sandwich.WebDriver.Internal.Binaries.Xvfb.Types (
  XvfbDependenciesSpec(..)
  , XvfbToUse(..)
  ) where

import Test.Sandwich.Contexts.Nix

data XvfbDependenciesSpec = XvfbDependenciesSpec {
  xvfbDependenciesSpecXvfb :: XvfbToUse
  }

-- | How to obtain the @xvfb-run@ binary.
data XvfbToUse =
  -- | Search the PATH for the @xvfb-run@ binary.
  UseXvfbFromPath
  -- | Use the @xvfb-run@ at the given path.
  | UseXvfbAt FilePath
  -- | Get @xvfb-run@ from Nixpkgs.
  | UseXvfbFromNixpkgs NixContext
  deriving Show
