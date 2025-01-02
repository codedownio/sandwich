---
id: intro
title: Welcome to Sandwich
sidebar_label: Introduction
slug: /
---

import useBaseUrl from "@docusaurus/useBaseUrl";
import "react-responsive-carousel/lib/styles/carousel.min.css";
import { Carousel } from "react-responsive-carousel";

import styles from "../src/pages/styles.module.css";

Sandwich is a test framework for Haskell, inspired by and (almost) a drop-in replacement for [Hspec](http://hspec.github.io/).

Sandwich has a number of powerful features and integrations, such as:
* Interactive terminal UI interface for viewing test progress and results.
* Built-in [profiling](/docs/profiling) support.
* Integrations such as [Hedgehog](/docs/extensions/sandwich-hedgehog), [QuickCheck](/docs/extensions/sandwich-quickcheck), and [Selenium](/docs/extensions/sandwich-webdriver).
* Context libraries that allow you to introduce things like [databases](/docs/context-libraries/sandwich-contexts#postgresql-contexts), [Docker containers](#TODO), or even full [Kubernetes clusters](/docs/context-libraries/sandwich-contexts-kubernetes).

<div className={styles.carouselContainer}>
  <Carousel showThumbs={false}
            dynamicHeight={true}
            statusFormatter={(current, total) => `${current} of ${total}`}>
    <div>
      <div className={styles.carouselHeading}>Terminal UI interface</div>
      <img src={useBaseUrl("/img/basic.gif")}
           style={{ paddingBottom: "2.05em" }} />
    </div>
    <div>
      <div className={styles.carouselHeading}>Jump to failure in editor</div>
      <img src={useBaseUrl("/img/jump_to_error.gif")}
           style={{ paddingBottom: "2.05em" }} />
    </div>
    <div>
      <div className={styles.carouselHeading}>Slack integration</div>
      <img src={useBaseUrl("/img/slack.gif")} />
    </div>
    <div>
      <div className={styles.carouselHeading}>Timing and flamegraphs</div>
      <img src={useBaseUrl("/img/timing_landing.gif")} />
    </div>
  </Carousel>
</div>

## Basic tests

Let's start with a basic test suite and add more features as we go along. As with other test frameworks, tests are structured as a **tree**, defined using a simple free monad with nodes like `describe` and `it`. There are a total of 8 such basic nodes and we'll see others as we go along.

The meat of the tests occurs in "it" nodes at the leaves of the tree. Every test runs in a special monad called `ExampleT`, which is essentially a `ReaderT context LoggingT`. The `LoggingT` part gives tests the ability to log information, and the `ReaderT` gives tests access to *context*. More on this later. The monad also implements some other useful classes like `MonadIO`, so you can run arbitrary IO actions.

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-basic/app/Main.hs"
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

The tests above assert things using expectation functions like `shouldBe`. There are a variety of these in [Test.Sandwich.Expectations](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Expectations.html) such as `shouldNotBe`, `shouldContain`, etc., and they are similar to other test frameworks.

These functions simply throw an exception of type [FailureReason](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Misc.html#t:FailureReason) which the Sandwich machinery catches and displays. Don't worry, you can throw other exceptions too. You can even write instances for your [custom exception types](formatters/tui#custom-exception-formatters) so that they display nicely in Sandwich [formatters](formatters/tui).

To fail a test with a string message, just call [expectationFailure](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Expectations.html#v:expectationFailure). You can also mark a test as "pending" by calling the [pending](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Expectations.html#v:pending) function anywhere in the test, or by changing `it` to `xit`.

## TUI interface

Let's run this test from the command line, using the [Terminal UI interface](/docs/formatters/tui). This will allow us to move around and examine the tests. In particular, we can examine the failure and log message in the subtraction tests.

Since we used [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs), we can pass flags to control the formatter:

```bash
~/sandwich> stack run demo-basic -- --tui
```

<video width="100%" controls autoplay="true" muted="true">
  <source src={useBaseUrl('img/basic_tui.webm')} type="video/webm"></source>
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
   └─ subtracts --> ../results/Arithmetic/subtracts # failure symlink
```

Thus, every test tree node has a place where it can stash logs, screenshots, or other artifacts. This structure makes it easy to browse through your tests and check results.

The `errors` folder at the root provides a handy list of symlinks to all failures.

> Check out the next sections to learn about contexts, hooks, and more!
