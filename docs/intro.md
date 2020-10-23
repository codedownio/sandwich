---
id: intro
title: Welcome to Sandwich
sidebar_label: Introduction
---

Sandwich is a test framework for Haskell, heavily inspired by and (almost) a drop-in replacement for [Hspec](http://hspec.github.io/).

It offers a set of powerful features for writing spec-style tests, including

* Parallel test execution
* Setup and teardown hooks
* An extensible *context* system for bracket-style introduction of dependencies
* Integrated logging and artifact collection
* Nicely formatted exceptions
* A range of powerful *formatters* for viewing and interacting with test results:
  * [`sandwich-slack`](sandwich-slack): report test progress to Slack, with live progress bars and test failures
  * [TUI interface](formatters/tui): full terminal UI app for viewing test progress & results, controlling & rerunning tests, etc.
* Other integrations
  * `sandwich-webdriver`: run Selenium tests with ease, including automatic downloading of driver binaries
  * `sandwich-quickcheck`: run quickcheck tests (coming soon)
  * `sandwich-smallcheck`: run smallcheck tests (coming soon)
