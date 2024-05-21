---
id: silent
title: Silent Formatter
---

import useBaseUrl from '@docusaurus/useBaseUrl';

The silent formatter prints only the run root of the test tree. It's useful when you don't want to see live output, since you'll just examine the results on disk later. It can also be used while testing in CI with the [Slack formatter](./slack) enabled.

<img alt="Silent Formatter output" src={useBaseUrl('img/silent.png')} />

## Usage

If you're using [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs), you can simply pass `--silent` and the silent formatter will be selected.

If you're using the lower-level [runSandwich](http://hackage.haskell.org/package/sandwich0.1.0.3/docs/Test-Sandwich.html#v:runSandwich), simply include the formatter in the [optionsFormatters](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Options.html#v:optionsFormatters) of your sandwich options.

```haskell
import Test.Sandwich.Formatters.Silent

main :: IO ()
main = runSandwich options myTests
  where
    options = defaultOptions {
      optionsFormatters = [SomeFormatter defaultSilentFormatter]
      }
```
