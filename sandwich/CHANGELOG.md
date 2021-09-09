# Changelog for sandwich

## Unreleased changes

## 0.1.0.9

* Expose `optionsDryRun` and the `--dry-run` option.
* Add command-line arguments for `sandwich-quickcheck`.
* Add `--visibility-threshold`/`-v` option.
* Fix an issue where TUI quit would hang in the presence of cleared results.
* Improve `runSandwichWithCommandLineArgs` to better detect when the TUI formatter can be run.
* Display number of tests run in output message.
* Catch and log exceptions in the TUI event loop.

## 0.1.0.8

* GHC 9 support

## 0.1.0.7

* Add `terminalUICustomExceptionFormatters`.

## 0.1.0.6

* Add `parallelN` for limiting the number of threads in a `parallel`.
