---
id: intro
title: Welcome to Sandwich
sidebar_label: Introduction
slug: /
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Sandwich is a test framework for Haskell, heavily inspired by and (almost) a drop-in replacement for [Hspec](http://hspec.github.io/). This section will show some of its features.

## Basic tests

Let's start with a basic test suite and add more fancy features as we go along. As with other test frameworks, tests are structured as a **tree**, defined using a simple free monad with nodes like `describe` and `it`. There are a total of 8 such basic nodes and we'll see others as we go along.

The meat of the tests occurs in "it" nodes at the leaves of the tree. Every test runs in a special monad called `ExampleT`, which is essentially a `ReaderT context LoggingT`. The `LoggingT` part gives test the ability to log information, and the `ReaderT` gives tests access to *context*. More on this later.

```haskell title="https://github.com/thomasjm/sandwich/blob/master/sandwich-demos/demos/basic/Main.hs"
module Main where

import Test.Sandwich

basic :: TopSpec
basic = describe "Simple tests" $ do
  describe "Arithmetic" $ do
    it "tests addition" $ do
      (2 + 2) `shouldBe` 4
      (2 + 3) `shouldBe` 5

    it "tests subtraction" $
      (3 - 2) `shouldBe` 1

  describe "Strings" $
    it "concatenates strings" $
      ("abc" <> "def") `shouldBe` "abcdef"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
```

Let's run this test from the command line:

<img alt="Simple tests" src={useBaseUrl('img/basic_print_and_tui.gif')} />



In our test file above, we define a `main` function using `runSandwichWithCommandLineArgs`, a convenience function that allows us to configure options via the command line. For example, we can choose between different *formatters*, which control how output is displayed. The video below shows running the tests with both the print and TUI formatters.



## Contexts




## Hooks

