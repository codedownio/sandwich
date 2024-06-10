
module Test.Sandwich.WebDriver.Internal.Binaries.Selenium.Types where

import Test.Sandwich.Contexts.Nix


-- | How to obtain the Selenium server JAR file.
data SeleniumToUse =
  DownloadSeleniumFrom String
  -- ^ Download selenium from the given URL to the 'toolsRoot'
  | DownloadSeleniumDefault
  -- ^ Download selenium from a default location to the 'toolsRoot'
  | UseSeleniumAt FilePath
  -- ^ Use the JAR file at the given path
  | UseSeleniumFromNixpkgs NixContext
  -- ^ Use the Selenium in the given Nixpkgs derivation
  deriving Show
