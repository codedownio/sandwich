---
id: node_options
title: Node Options
sidebar_label: Node Options
---

The basic functions like [describe](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:describe), [it](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:it), etc. are aliases to lower-level functions called [describe'](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Nodes.html#v:describe-39-), [it'](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Nodes.html#v:it-39-), etc. The lower-level versions accept [node options](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Nodes.html#t:NodeOptions) which you can use to fine-tune how each node behaves.

## Visibility thresholds

In a given test tree, some nodes are usually more "interesting" for reporting purposes than others. Nodes like `before` and `parallel` are more about controlling setup and semantics, so you may not care about them as much as `describe` and `it` nodes. (Which is not to say these nodes are never interesting; if a `before` node throws an exception, you still want the ability to examine it.)

For example, consider some tests that initialize a server and a database, then run some tests in parallel. Which of the following failure reports is more useful?

* before (set up server), before (initialize database), describe "server database", parallel, it "tests table 1"
* describe "server database tests", it "tests table 1"

The latter makes it easier to quickly pick out which test failed.

To allow this kind of filtering, every test node has a number associated with it called its *visibility threshold*. This is a number from `0` to `Infinity` representing the visual "priority" of a node. Sandwich formatters are designed so that you can set a threshold such that only nodes whose value falls under the threshold are displayed. This gives us the ability to have deeply nested and complex test trees, but prune away the complexity when desired.

The default visibility thresholds for each node is as follows:

- `0`: `it`
- `50`: `describe`
- `70`: `parallel`
- `100`: `introduce`, `before`, `after`, `around` (and their `*each` and `*with` versions)
- `150`: various automatically-introduced nodes, like those added for timing

If you want to set *custom* visibility thresholds, you can use the primed versions of the node functions to do it. For example:

```haskell
-- | A version of 'before' that has a lower visibility threshold
myBefore = before' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 50 })
```

## Folders

By default, every node in the test tree will get an associated folder in the [on-disk results](/docs/#on-disk-results). This folder is where logs and other artifacts related to the node will be stored.

You can disable this by setting [nodeOptionsCreateFolder](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Nodes.html#v:nodeOptionsCreateFolder) to `False`. This is useful to do for nodes that don't produce any useful artifacts on disk. For example, this is done for test nodes related to internal timing functionality so that they don't clutter the on-disk results. Be careful -- if you disable the on-disk folder for a node and that node throws an exception, it could be harder to debug.

## Timing

By default, every node in the test tree will be timed using the configured test timer. To turn this off for an individual node, you can set [nodeOptionsRecordTime](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Nodes.html#v:nodeOptionsRecordTime) to `False`. (Most often, you would want to do this if you're working with parallel tests and want to prevent a racy node from recording a stack frame in the test timings.)
