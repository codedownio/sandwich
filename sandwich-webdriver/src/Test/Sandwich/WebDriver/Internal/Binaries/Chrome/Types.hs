
module Test.Sandwich.WebDriver.Internal.Binaries.Chrome.Types (
  ChromeToUse(..)
  , ChromeDriverToUse(..)
  , ChromeVersion(..)
  , ChromeDriverVersion(..)
  ) where

import Data.Text as T
import Test.Sandwich.Contexts.Nix

-- | How to obtain the chrome binary.
data ChromeToUse =
  -- | Search the PATH for the "google-chrome" or "google-chrome-stable" binary.
  UseChromeFromPath
  -- | Get Chrome from Nixpkgs
  | UseChromeFromNixpkgs NixContext
  deriving Show

-- | How to obtain the chromedriver binary.
data ChromeDriverToUse =
  DownloadChromeDriverFrom String
  -- ^ Download chromedriver from the given URL to the 'toolsRoot'
  | DownloadChromeDriverVersion ChromeDriverVersion
  -- ^ Download the given chromedriver version to the 'toolsRoot'
  | DownloadChromeDriverAutodetect FilePath
  -- ^ Autodetect chromedriver to use based on the Chrome version and download it to the 'toolsRoot'
  -- Pass the path to the Chrome binary, or else it will be found by looking for google-chrome on the PATH.
  | UseChromeDriverAt FilePath
  -- ^ Use the chromedriver at the given path
  | UseChromeDriverFromNixpkgs NixContext
  -- ^ Use the chromedriver in the given Nixpkgs derivation
  deriving Show

newtype ChromeVersion = ChromeVersion (Int, Int, Int, Int) deriving Show
data ChromeDriverVersion =
  ChromeDriverVersionTuple (Int, Int, Int, Int)
  | ChromeDriverVersionExactUrl (Int, Int, Int, Int) Text
  deriving Show
