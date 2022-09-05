# Changelog for sandwich

## Unreleased changes

## 0.1.0.11

* Initial release Test.Sandwich.Golden for golden testing.
* Support Brick 1.x in addition to 0.x.

## 0.1.0.10

* Add Markdown Summary formatter (useful with GitHub Actions)
* Release `sandwich-hedgehog` compatibility.

## 0.1.0.9

* Expose `optionsDryRun` and the `--dry-run` option.
* Add command-line arguments for `sandwich-quickcheck`.
* Add `--visibility-threshold`/`-v` option.
* Fix an issue where TUI quit would hang in the presence of cleared results.
* Display number of tests run in output message.
* Catch and log exceptions in the TUI event loop.
* Change `runSandwichWithCommandLineArgs` to use the print formatter by default, since we can't figure out how to detect if we're running under `cabal test`, which redirects stdout.

## 0.1.0.8

* GHC 9 support

## 0.1.0.7

* Add `terminalUICustomExceptionFormatters`.

## 0.1.0.6

* Add `parallelN` for limiting the number of threads in a `parallel`.
