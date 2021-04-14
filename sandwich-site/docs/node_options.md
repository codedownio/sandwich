---
id: node_options
title: Node Options
sidebar_label: Node Options
---

## Introduction

The basic functions like `describe`, `it`, etc. are aliases to lower-level functions called `describe'`, `it'`, etc. The lower-level versions accept **node options** which you can use to fine-tune how each node behaves.

## Visibility thresholds

In a given test tree, some nodes are usually more "interesting" for reporting purposes than others. Nodes like `before` and `parallel` are more about controlling setup and semantics, so you may not care about them as much as `describe` and `it` nodes. (Which is not to say these nodes are never interesting; if a `before` node throws an exception, you still want the ability to examine it.)

For example, consider some tests that initialize a server and a database, then run some tests in parallel. Which of the following failure reports is more useful?

* before (set up server), before (initialize database), describe "server database", parallel, it "tests table 1"
* describe "server database tests", it "tests table 1"

The latter makes it easier to quickly pick out which test failed.

To allow this kind of filtering, every test node has a number associated with it called its *visibility threshold*. This is a number from `0` to `Infinity` representing the visual "priority" of a node. Sandwich formatters are designed so that you can set or toggle a threshold such that only nodes whose value falls under the threshold are displayed. This gives us the ability to have deeply nested and complex test trees, but prune away the complexity when desired.

The default visibility thresholds for each node is as follows:

- `0`: `it`
- `50`: `describe`
- `70`: `parallel`
- `100`: `introduce`, `before`, `after`, `around` (and their `*each` and `*with` versions)

If you want to set *custom* visibility thresholds, you can use the primed versions of the node functions to do it. For example:

```haskell
-- | A version of 'before' that has a lower visibility threshold
myBefore = before' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 50 })
```

## Folder creation

By default, every node in the test tree will get an associated folder in the [on-disk results](/docs#on-disk-results).
