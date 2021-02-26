---
id: test_trees
title: Test Trees
sidebar_label: Test Trees
---

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
