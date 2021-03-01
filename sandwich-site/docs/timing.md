---
id: timing
title: Timing
sidebar_label: Timing
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Sandwich has a built-in notion of *test timing*. Timing can be useful when you have a large test suite and want to understand where it is spending the most time.

The goal of the timing system is to create a nice summary report, ideally in the form of a [flame graph](http://www.brendangregg.com/flamegraphs.html).

You can select a test timer implementation you use in the Sandwich options. The default one targets [SpeedScope](https://www.speedscope.app/), a nice web-based tool for visualizing flame graphs.


### Using the default profile

The simplest use of timing occurs when we don't have any `parallel` stuff going on, so the tests all run in a single thread. To deal with multiple threads, see the [profiles](#profiles) section.

First of all, every node in the test tree is timed by default. Thus, the "describe" and "it" nodes in the example below will be timed. You can prevent this by changing the [node options](TODO).

In addition, you can time arbitrary blocks of code using the `timeAction` function. This function is a `bracket_` style combinator that can be used to wrap an action. In the example below, we use it to wrap some sub-steps within a test.

``` haskell title="https://github.com/thomasjm/sandwich/blob/master/sandwich-demos/demos/timing/Main.hs"
timingDemo :: TopSpec
timingDemo = describe "Dinner tests" $ do
  it "Makes dinner" $ do
    pauseSeconds 1
    timeAction "Makes pasta" $ do
      timeAction "Heats water" $ pauseSeconds 1
      timeAction "Boils noodles" $ pauseSeconds 0.8
      timeAction "Decants noodles" $ pauseSeconds 0.7

  it "Cleans up" $ do
    pauseSeconds 1
```

When you run this code using the default implementation, it will output a file `speedscope.json` in the root of the test results. If you drag and drop this file onto SpeedScope, you get a picture like this:

<img alt="Simple timing example" src={useBaseUrl('img/dinner_timing.png')} />


## Dealing with concurrency

Flame graphs need to be properly *nested* to be valid. If Frame A starts before Frame B, then Frame B must end before Frame A ends. When you run test subtrees in parallel, it's easy to violate this property and get stack frames that cross over each other. This will result in a malformed JSON file that makes the visualizer unhappy.

The solution is to introduce "profiles" within the test timer to correspond to execution threads, and make sure you run in a single-threaded way *within each profile*. Here's a simple example of this in action:

```haskell
timingParallelDemo :: TopSpec
timingParallelDemo = parallel $ do
  withTimingProfile "italian" $
    it "Makes Italian dinner" $ do
      pauseSeconds 1
      timeAction "Makes pasta" $ do
        timeAction "Heats water" $ pauseSeconds 1
        timeAction "Boils noodles" $ pauseSeconds 0.8
        timeAction "Decants noodles" $ pauseSeconds 0.7

  withTimingProfile "chinese" $
    it "Makes Chinese dinner" $ do
      pauseSeconds 1
      timeAction "Makes rice" $ do
        timeAction "Washes rice" $ pauseSeconds 1
        timeAction "Starts steamer" $ pauseSeconds 0.8
        timeAction "Serves rice" $ pauseSeconds 0.7
```

<img alt="Parallel timing example" src={useBaseUrl('img/timing_parallel.gif')} />


## The timing functions

You can introduce timing either at the `Spec` level or at the test/`ExampleT` level. Doing it at the spec level involves introducing a node in your test tree whose entire purpose is to measure the time of its subtree. Doing it at the test level involves wrapping an action in a `bracket_` style function that records the timing. These options are explained in more detail below.



### Using a custom profile


