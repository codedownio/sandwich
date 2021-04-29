---
id: print
title: Print Formatter
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The print formatter gives traditional print output, similar to other test tools. The tree is printed from top to bottom, with each node of the test tree colored based on whether it succeeded or failed. It also shows information about failures and log messages.

:::note
The print formatter works fine with parallel tests! It will always print from top to bottom as results become available, but don't worry -- if you have multiple threads they are still running in the background.
:::

<img alt="Print Formatter output" src={useBaseUrl('img/print.png')} />

## Usage

If you're using [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs), you can simply pass `--print` and the print formatter will be selected.

If you're using the lower-level [runSandwich](http://hackage.haskell.org/package/sandwich0.1.0.3/docs/Test-Sandwich.html#v:runSandwich), simply include the formatter in the [optionsFormatters](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Options.html#v:optionsFormatters) of your sandwich options.

```haskell
import Test.Sandwich.Formatters.Print

main :: IO ()
main = runSandwich options myTests
  where
    options = defaultOptions {
      optionsFormatters = [SomeFormatter defaultPrintFormatter]
      }
```

## Configuration

Like other formatters, you can adjust the [visibility threshold](#). You can also change the [log level](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-Print.html#v:printFormatterLogLevel) and set whether or not to include [callstacks](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-Print.html#v:printFormatterIncludeCallStacks).

You can see the configuration options in the [documentation](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-Print.html).
