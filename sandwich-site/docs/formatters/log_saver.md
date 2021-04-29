---
id: log_saver
title: Log Saver Formatter
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The log saver formatter writes test logs to a given path on disk. Note that Sandwich already writes logs to disk by default, as configured by the [optionsSavedLogLevel](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Options.html#v:optionsSavedLogLevel) option, so this is primarily useful when you want to save logs to another location, or with a different log level, or with a different formatter from the global one.

It is a "secondary" formatter in the sense that it doesn't write to `stdout` or `stderr`, so it can run in the background while another formatter (such as the terminal UI or print formatters) monopolize the output streams.

## Usage

This formatter must be included manually in the [optionsFormatters](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Options.html#v:optionsFormatters) of your sandwich options.

```haskell
import Test.Sandwich.Formatters.LogSaver

main :: IO ()
main = runSandwich options myTests
  where
    options = defaultOptions {
      optionsFormatters = [SomeFormatter defaultLogSaverFormatter]
      }
```

## Configuration

The main option to set is the [logSaverPath](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-LogSaver.html#v:logSaverPath), which controls where the logs are written. This can be set to either an absolute path, or a path relative to the test run root.

You can see the other configuration options in the [documentation](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Formatters-LogSaver.html).
