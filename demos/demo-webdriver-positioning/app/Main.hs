{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Maybe
import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver
import Test.Sandwich.WebDriver.Windows
import Test.WebDriver as W
import Test.WebDriver.Capabilities
import Test.WebDriver.Commands


positioning :: TopSpecWithOptions
positioning = introduceNixContext (nixpkgsMaster { nixpkgsDerivationAllowUnfree = True }) $
  introduceWebDriverViaNix (defaultWdOptions { modifyCapabilities = setNoWayland }) $ do
    describe "two windows side by side" $ do
      it "opens Google" $ withSession1 $ do
        openPage "http://www.google.com"
        setWindowLeftSide

      it "opens xkcd" $ withSession2 $ do
        openPage "http://www.xkcd.com"
        setWindowRightSide

      it "pauses" $ do
        liftIO $ threadDelay 5000000

-- | Browsers can't position themselves when running under Wayland, so we force
-- X11 for Chrome and Firefox.
setNoWayland :: W.Capabilities -> IO W.Capabilities
setNoWayland caps = caps
  -- Chrome 140 switched to use Wayland by default.
  & over (capabilitiesGoogChromeOptions . _Just . chromeOptionsArgs)
         (Just . ("--ozone-platform=x11" :) . fromMaybe mempty)

  & over (capabilitiesMozFirefoxOptions . _Just . firefoxOptionsEnv)
    (Just . (M.insert "MOZ_ENABLE_WAYLAND" "0") . fromMaybe mempty)

  & return

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions positioning
