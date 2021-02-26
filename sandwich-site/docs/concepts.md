---
id: concepts
title: Core Concepts
sidebar_label: Concepts
---

## Test tree nodes

Sandwich is built around the idea of a *test tree*. You write a test tree using simple monadic syntax. The simplest kinds of nodes are `describe` and `it` nodes, which are used to group and define tests. For example:

```haskell
tests = describe "arithmetic" $ do
  it "adds" $ do
    (2 + 2) `shouldBe` 4

  it "subtracts" $ do
    warn "Having some trouble getting this test to pass..."
    (2 - 2) `shouldBe` 1
```

However, several other node types also exist:

* `parallel` node: runs all of its child nodes in parallel.
* `before`/`beforeEach` runs an action before its child nodes.
* `after`/`afterEach` runs an action after its child nodes.
* `around`/`aroundEach` wrap the child nodes with a callback.
* `introduce`/`introduceWith` supply a *context* to its child nodes.

## Visibility thresholds

In a given test tree, some nodes are usually more "interesting" for reporting purposes than others. Nodes like `before` and `parallel` are more about controlling setup and semantics, so you may not care about them as much as `describe` and `it` nodes. (Which is not to say these nodes can't fail; if a `before` node throws an exception, you still want the ability to examine it.)

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

## On-disk test tree

Unless configured otherwise, each test tree run produces a *directory tree* which exactly mirrors the tree structure. For example, the test tree above would produce a tree like the following.

```bash
<test_root>
├─ results
│  └─ arithmetic
│     ├─ adds
│     └─ subtracts
│        └─ test_logs.txt # contains the log warning message
└─ errors
   └─ subracts --> ../results/subtracts # failure symlink
```

Thus, every test tree node has a place where it can stash logs, screenshots, or other artifacts. This structure makes it easy to browse through your tests and check things. Also, the `errors` folder at the root provides a handy list of symlinks to all failures.

## Monad transformer

Every test (as well as the callbacks in functions like `around`) runs within a monad transformer stack called `ExampleT`. This stack provides the logging, contexts, test folder access, etc. for your tests. It implements `MonadIO` so you can run any `IO` actions you like in it.
