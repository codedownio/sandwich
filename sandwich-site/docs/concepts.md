---
id: concepts
title: Core Concepts
sidebar_label: Concepts
---

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
