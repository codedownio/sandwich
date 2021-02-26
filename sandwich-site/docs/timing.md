---
id: timing
title: Timing
sidebar_label: Timing
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Sandwich has a built-in notion of *test timing*. Timing can be useful when you have a large test suite and want to understand where it is spending the most time.

The goal of the timing system is to create a nice summary report, ideally in the form of a [flame graph](http://www.brendangregg.com/flamegraphs.html).

You can select a test timer implementation you use in the Sandwich options. The default one targets [SpeedScope](https://www.speedscope.app/), a nice online tool for visualizing flame graphs.


### Using the default profile

The simplest use of timing occurs when we don't have any `parallel` stuff going on, so the tests all run in a single thread. To deal with multiple threads, see the [profiles](#profiles) section.

The simplest test function are `timingNode` and `timeAction`, respectively. The former creates a node in your test tree representing the timing, while the latter is more a general `bracket_` style combinator that can be used to wrap an action.

``` haskell
timingDemo :: TopSpec
timingDemo = do
  it "Makes dinner" $ timingNode "Makes dinner" $ do
    pauseSeconds 1
    timeAction "Makes pasta" $ do
      timeAction $ "Heats water" $ pauseSeconds 1
      timeAction $ "Boils noodles" $ pauseSeconds 1
      timeAction $ "Decants noodles" $ pauseSeconds 0.5

  it "Cleans up" $ timingNode "Cleans up" $ do
    pauseSeconds 1
```

When you run this code using the default implementation, it will output a file `speedscope.json` in the root of the test results. If you drag and drop this file onto SpeedScope, you get a picture like this:

<img alt="Simple timing example" src={useBaseUrl('img/dinner_timing.png')} />


## Dealing with concurrency

Flame graphs need to be properly *nested* to be valid. If Frame A starts before Frame B, then Frame B must end before Frame A ends. When you run test subtrees in parallel, it's easy to violate this property and get stack frames that cross over each other. This will result in a malformed JSON file that makes the visualizer unhappy.

The solution is to introduce "profiles" within the test timer to correspond to execution threads, and make sure you run in a single-threaded way *within each profile*. Here's a simple example of this in action:

```haskell

```


## The timing functions

You can introduce timing either at the `Spec` level or at the test/`ExampleT` level. Doing it at the spec level involves introducing a node in your test tree whose entire purpose is to measure the time of its subtree. Doing it at the test level involves wrapping an action in a `bracket_` style function that records the timing. These options are explained in more detail below.



### Using a custom profile


