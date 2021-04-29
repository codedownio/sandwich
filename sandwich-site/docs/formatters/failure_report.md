---
id: failure_report
title: Failure Report Formatter
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The failure report formatter is similar to the print formatter, but it only prints failures. It's useful when you want less verbose output that only prints if something goes wrong. Like with the print formatter, it shows log messages, callstacks, and failure info in its messages.

<img alt="Failure report formatter output" src={useBaseUrl('img/failure_report.png')} />

## Usage

If you're using [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs), you can simply pass `--print-failures` and the failure report formatter will be selected.

If you're using the lower-level [runSandwich](http://hackage.haskell.org/package/sandwich0.1.0.3/docs/Test-Sandwich.html#v:runSandwich), simply include the formatter in the [optionsFormatters](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Options.html#v:optionsFormatters) of your sandwich options.

```haskell
import Test.Sandwich.Formatters.FailureReport

main :: IO ()
main = runSandwich options myTests
  where
    options = defaultOptions {
      optionsFormatters = [SomeFormatter defaultFailureReportFormatter]
      }
```

## Configuration

Like other formatters, you can adjust the [visibility threshold](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-FailureReport.html#v:failureReportLogLevel). You can also change the [log level](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-FailureReport.html#v:failureReportLogLevel) and set whether or not to include [callstacks](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-FailureReport.html#v:failureReportFormatterIncludeCallStacks).

You can see the configuration options in the [documentation](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-FailureReport.html).
