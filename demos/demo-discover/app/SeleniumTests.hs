{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module SeleniumTests where

import Test.Sandwich
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.WebDriver

#insert_test_imports


spec :: TopSpecWithOptions
spec = introduceNixContext (nixpkgsReleaseDefault { nixpkgsDerivationAllowUnfree = True }) $ introduceWebDriverViaNix defaultWdOptions $ do
  $(getSpecFromFolder defaultGetSpecFromFolderOptions)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
