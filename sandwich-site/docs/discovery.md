---
id: discovery
title: Test Discovery
sidebar_label: Test Discovery
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Test discovery is the process of automatically finding test files, so you don't need to manually manage your imports and write out top-level test trees.

For the purposes of this discussion, let's assume a somewhat complex test suite with different kinds of tests, laid out on disk like the following. The key point is that different groups of tests may require different contexts: for example, `UnitTests` have no dependencies but `SeleniumTests` require [Selenium](./extensions/sandwich-webdriver) context.

```bash
<project root>
└─ tests
   ├─ Main.hs
   ├─ UnitTests.hs
   ├─ UnitTests
   │  ├─ UnitTests1.hs
   │  └─ UnitTests2.hs
   ├─ SeleniumTests.hs
   └─ SeleniumTests
      ├─ SeleniumTests1.hs
      └─ SeleniumTests2.hs
```

In `Main.hs`, we want to automatically generate something like this:

```haskell
module Main where

import Test.Sandwich
import Test.Sandwich.WebDriver

import qualified UnitTests.UnitTests1
import qualified UnitTests.UnitTests2
import qualified SeleniumTests.SeleniumTests1
import qualified SeleniumTests.SeleniumTests2

tests = do
  describe "Unit tests" $ do
    UnitTests1.tests
    UnitTests2.tests

  introduceWebDriver (defaultWdOptions "/tmp/tools") $
    describe "Selenium tests" $ do
      SeleniumTests1.tests
      SeleniumTests2.tests
```

## The basic setup

Autogenerating the tests in a given module requires two pieces of code in that module: 1) A CPP pragma to generate the imports, and 2) a Template Haskell call to generate the test tree. (Unfortunately it can't be done solely with Template Haskell, because [TH cannot generate imports](https://gitlab.haskell.org/ghc/ghc/-/issues/1475).)

To autogenerate tests for the example above, we'll apply autodetection separately in `UnitTests.hs` and `SeleniumTests.hs`. You can follow along with the full example [here](https://github.com/codedownio/sandwich/tree/master/demo-discover).

In the code below, the `OPTIONS_GHC` pragma invokes the `sandwich-discover` executable, which searches for modules *underneath the current module* (i.e., matching `SeleniumTests.*`). Then it inserts the imports wherever it finds the special `#insert_test_imports` token.

Finally, the code below calls `getSpecFromFolder` in a TH spec to generate the actual test tree.

```haskell title="SeleniumTests.hs"
{-# OPTIONS_GHC -F -pgmF sandwich-discover #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module SeleniumTests where

import Test.Sandwich
import Test.Sandwich.WebDriver

#insert_test_imports

tests :: TopSpec
tests = describe "Selenium tests" $ introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  $(getSpecFromFolder defaultGetSpecFromFolderOptions)

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
```

Once we write similar boilerplate in `UnitTests.hs`, we can pull both sub-trees together into the main top-level tree below.

```haskell title="Main.hs"
module Main (Main.main) where

import qualified SeleniumTests
import qualified UnitTests

discoverDemo :: TopSpec
discoverDemo = describe "Discover" $ do
  UnitTests.tests
  SeleniumTests.tests
```

## Running individual test modules

Having set up test autodetection as above, we can now take advantage of the ability to run individual test modules. When you run with `--list-tests`, you'll see a list of special flags you can pass. When you pass any of these flags, Sandwich will run only that test module.

```bash
> stack run demo-discover -- --list-tests

Available options:
  --selenium-tests         SeleniumTests
  --selenium-tests1        SeleniumTests.SeleniumTests1
  --selenium-tests2        SeleniumTests.SeleniumTests2
  --unit-tests             UnitTests
  --unit-tests1            UnitTests.UnitTests1
  --unit-tests2            UnitTests.UnitTests2
  -h,--help                Show this help text
```


:::note
You can always run individual test subtrees by simply passing `--filter "some filter string"` with an appropriate filter string. However, this will filter the tree to any nodes that match the filter string, so it may not be as convenient to exactly match the subtree corresponding to a single module.
:::


## Main function autodetection

Sometimes you want to include a `main` function in an individual test module. These main functions can be convenient when you want to iterate on a single test module within a GHCi session, for example.

Sandwich can discover the presence of these `main` functions and give you the ability to run them using the individual module flag. It does this using some [magic](https://hackage.haskell.org/package/haskell-src-exts).

If this has happened, Sandwich will indicate it by putting an asterisk next to the module name. For example, if `UnitTests2.hs` had its own main function inside, you would see the following.

```bash
> stack run demo-discover -- --list-tests

Available options:
  --selenium-tests         SeleniumTests
  --selenium-tests1        SeleniumTests.SeleniumTests1
  --selenium-tests2        SeleniumTests.SeleniumTests2
  --unit-tests             UnitTests
  --unit-tests1            UnitTests.UnitTests1
  --unit-tests2            UnitTests.UnitTests2*
  -h,--help                Show this help text
```

Thus, passing `--unit-tests2` would result in that file's `main` function being invoked, rather than the normal one in `Main.hs`.
