---
id: discovery
title: Test Discovery
sidebar_label: Test Discovery
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Test discovery is the process of automatically finding test files, so you don't need to have to manually manage your imports and write out a top-level test-tree.

For the purposes of this discussion, let's assume a somewhat complex test suite with different kinds of tests, laid out on disk like this:

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

Autogenerating this stuff requires two pieces of code in your file:

* A CPP pragma to generate the imports, and
* A Template Haskell call to generate the test tree

Unfortunately it can't all be done with Template Haskell, because [TH cannot generate imports](https://gitlab.haskell.org/ghc/ghc/-/issues/1475).

## Listing available tests

## Main function autodetection
