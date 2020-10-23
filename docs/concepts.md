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

## On-disk test tree

Unless configured otherwise, each test tree run produces a *directory tree* which exactly mirrors the tree structure. For example, the test tree above would produce a tree like the following.

```bash
<test_root>
├─ results
│  └─ arithmetic
│     ├─ adds
│     └─ subtracts
│        └─ test_logs.txt # contains the log warning message
├─ errors
   └─ subracts --> ../results/subtracts # failure symlink
```

Thus, every test tree node has a place where it can stash logs, screenshots, or other artifacts. This structure makes it easy to browse through your tests and check things. Also, the `errors` folder at the root provides a handy list of symlinks to all failures.

## Monad transformer

Every test (as well as the callbacks in functions like `around`) runs within a monad transformer stack called `ExampleT`. This stack provides the logging, contexts, test folder access, etc. for your tests. It implements `MonadIO` so you can run any `IO` actions you like in it.
