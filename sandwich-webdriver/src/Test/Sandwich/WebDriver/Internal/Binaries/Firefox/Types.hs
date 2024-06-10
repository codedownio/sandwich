
module Test.Sandwich.WebDriver.Internal.Binaries.Firefox.Types where

import Test.Sandwich.Contexts.Nix


-- | How to obtain the firefox binary.
data FirefoxToUse =
  -- | Search the PATH for the "firefox" binary.
  UseFirefoxFromPath
  -- | Get Firefox from Nixpkgs
  | UseFirefoxFromNixpkgs NixContext
  deriving Show

-- | How to obtain the geckodriver binary.
data GeckoDriverToUse =
  DownloadGeckoDriverFrom String
  -- ^ Download geckodriver from the given URL to the 'toolsRoot'
  | DownloadGeckoDriverVersion GeckoDriverVersion
  -- ^ Download the given geckodriver version to the 'toolsRoot'
  | DownloadGeckoDriverAutodetect
  -- ^ Autodetect geckodriver to use based on the Firefox version and download it to the 'toolsRoot'.
  | UseGeckoDriverAt FilePath
  -- ^ Use the geckodriver at the given path
  | UseGeckoDriverFromNixpkgs NixContext
  -- ^ Use the geckodriver in the given Nixpkgs derivation
  deriving Show

newtype FirefoxVersion = FirefoxVersion (Int, Int, Int) deriving Show
newtype GeckoDriverVersion = GeckoDriverVersion (Int, Int, Int) deriving Show
