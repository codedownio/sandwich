---
id: intro
title: Welcome to Sandwich
sidebar_label: Introduction
slug: /
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Sandwich is a test framework for Haskell, heavily inspired by and (almost) a drop-in replacement for [Hspec](http://hspec.github.io/). This section will show some of its features.

## Basic tests

Let's start with a basic test suite and add more features as we go along. As with other test frameworks, tests are structured as a **tree**, defined using a simple free monad with nodes like `describe` and `it`. There are a total of 8 such basic nodes and we'll see others as we go along.

The meat of the tests occurs in "it" nodes at the leaves of the tree. Every test runs in a special monad called `ExampleT`, which is essentially a `ReaderT context LoggingT`. The `LoggingT` part gives test the ability to log information, and the `ReaderT` gives tests access to *context*. More on this later. The monad also implements some other useful classes like `MonadIO`, so you can run arbitrary IO actions.

```haskell title="https://github.com/thomasjm/sandwich/blob/master/sandwich-demos/demos/basic/Main.hs"
module Main where

import Test.Sandwich

basic :: TopSpec
basic = do
  describe "Arithmetic" $ do
    it "adds" $ do
      (2 + 2) `shouldBe` 4
      (2 + 3) `shouldBe` 5

    it "subtracts" $ do
      (3 - 2) `shouldBe` 0
      warn "This test might not be right..."

  describe "Strings" $
    it "concatenates" $
      ("abc" <> "def") `shouldBe` "abcdef"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic
```

## Expectations



## TUI interface

Let's run this test from the command line, using the [Terminal UI interface](/docs/formatters/tui). This will allow us to move around and examine the tests. In particular, we can examine the failure and log message in the subtraction tests.

Since we used `runSandwichWithCommandLineArgs`, we can pass flags to control the formatter:

```bash
~/sandwich> stack run basic -- --tui
```

<video width="100%" controls autoplay="true" muted="true">
  <source src="/img/basic_tui.webm" type="video/webm"></source>
Your browser does not support the video tag.
</video>

## On-disk results

Unless configured otherwise, each test tree run produces a *directory tree* which exactly mirrors the test tree structure. For example, the test tree above would produce a tree like the following.

```bash
<test_root>
├─ results
│  ├─ Arithmetic
│  │  ├─ adds
│  │  └─ subtracts
│  │     └─ test_logs.txt # contains the log warning message
│  └─ Strings
│     └─ concatenates
└─ errors
   └─ subtracts --> ../results/subtracts # failure symlink
```

Thus, every test tree node has a place where it can stash logs, screenshots, or other artifacts. This structure makes it easy to browse through your tests and check results. Also, the `errors` folder at the root provides a handy list of symlinks to all failures.

> Check out the next sections to learn about contexts, hooks, and more!
