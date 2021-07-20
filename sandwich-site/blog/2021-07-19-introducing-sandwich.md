---
slug: introducing-sandwich
title: Introducing Sandwich
author: Tom McLaughlin
author_title: Author
author_url: https://github.com/thomasjm
author_image_url: https://avatars.githubusercontent.com/u/1634990?v=4
tags: []
---


It's been a little over a year since I made the first commit on a new test framework. At the time I was working with a large and complex test suite, starting and stopping lots of Docker containers of different kinds and running Selenium tests against them. Several pieces of this infrastructure had dependencies on other pieces, and they needed to be started up and torn down gracefully, especially when one piece had a problem. It was essential to be able to quickly diagnose why some component was freezing and hanging the tests.

But why write a new test framework? Working with existing frameworks, I found myself wanting several things:

* **More visibility into and control over parallelism.** I wanted to be able to easily see how many threads were running and what they were doing. This led to creating the terminal UI interface, and other reporting tools like the Slack integration and test timing capabilities.

* **More logging and tracing capabilities.** So I integrated `MonadLogger` capabilities into the test tree and created an on-disk representation where test logs and artifacts could be organized.

* **Easier management of contexts.** I had some [ideas](https://hackage.haskell.org/package/sandwich-0.1.0.8/docs/Test-Sandwich.html#v:introduce) about how to use type-level nonsense to manage contexts in the test tree in such a way that they're keyed by a certain label, which tests can retrieve when needed. Also, it's a small thing, but I wanted access to a full monad transformer stack in an introduce node (Hspec hooks just use IO).

* **Better UX for complex test trees.** When you have a lot of test tree nodes devoted to starting and stopping dependencies, timing things, and other bookkeeping, it can be hard to see the actual tests in the output. This was solved with the creation of [visibility thresholds](https://codedownio.github.io/sandwich/docs/node_options#visibility-thresholds).

* **Integrations** with certain things I use heavily, namely Selenium. I had built up a fair amount of machinery for running Selenium tests, and it made sense to integrate it with the test suite.

Undoubtedly I could have gotten a lot of this by continuing to work with Hspec and Tasty, which are both great tools. Was it worth it? Personally I've been using it happily for a while now, but you be the judge :)


---


About the name: while designing this I was thinking about how to expose the [bracket](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:bracket) pattern in a test node such as [introduceWith](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:introduceWith). Around the same time, California opened up outdoor dining for the first time during the Covid lockdowns, and I had a really good sandwich on a beautiful day outside. Maybe because it was the first respite in a while from my own cooking, but that sandwich stuck in my mind.
