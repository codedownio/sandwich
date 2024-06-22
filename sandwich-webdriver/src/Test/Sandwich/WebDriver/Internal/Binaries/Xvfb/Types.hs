
module Test.Sandwich.WebDriver.Internal.Binaries.Xvfb.Types (
  XvfbDependenciesSpec(..)
  , XvfbToUse(..)
  , FluxboxToUse(..)
  ) where

import Test.Sandwich.Contexts.Nix

data XvfbDependenciesSpec = XvfbDependenciesSpec {
  xvfbDependenciesSpecXvfb :: XvfbToUse
  , xvfbDependenciesSpecFluxbox :: Maybe FluxboxToUse
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

-- | How to obtain the @fluxbox@ binary.
data FluxboxToUse =
  -- | Search the PATH for the @fluxbox@ binary.
  UseFluxboxFromPath
  -- | Use the @fluxbox@ at the given path.
  | UseFluxboxAt FilePath
  -- | Get @fluxbox@ from Nixpkgs.
  | UseFluxboxFromNixpkgs NixContext
  deriving Show
