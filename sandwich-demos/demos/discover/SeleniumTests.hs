{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SeleniumTests where

import Test.Sandwich
import Test.Sandwich.WebDriver

#insert_test_imports


tests :: TopSpec
tests = describe "Selenium tests" $ introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  $(getSpecFromFolder defaultGetSpecFromFolderOptions)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
