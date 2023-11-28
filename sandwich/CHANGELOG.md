# Changelog for sandwich

## Unreleased changes

## 0.2.1.0

* Improve clock management; don't keep incrementing it when nothing and restart it when r/R are pressed.

## 0.2.0.0

* Allow any formatter except TUI to be used with --repeat N.
* Be able to include timestamps with print formatter and failure report formatter.
* Support vty-6.x/brick-2.x. This change adds Windows support, but forces us to do a major version bump.
* Add timing info for setup and teardown; closes #10

## 0.1.5.2

* Contexts: add pushContext and popContext helpers.

## 0.1.5.1

* Logging: add readCreateProcessWithLogging

## 0.1.5.0

* GHC 9.6 support

## 0.1.4.0

* Windows improvements (fix "invalid argument (invalid character)", fix console unicode output)
* Add Alternative (ExampleT context m) instance

## 0.1.3.2

* Prevent spurious messages in IOExceptions from withFile

## 0.1.3.1

* Fix #61 (options not being passed from configured TUI formatter when `--tui` flag is used)

## 0.1.3.0

* Add the --prune option (#69)

## 0.1.2.0

* Be able to control `sandwich-webdriver` download directory.
* Add flags to control `sandwich-webdriver` Selenium paths: `--selenium-jar`, `--chrome-binary`, `--chromedriver-binary`, `--firefox-binary`, `--geckodriver-binary`.

## 0.1.1.2

* Improve semantics of multiple `--filter/-f` arguments. Now they get applied to the test tree sequentially.

## 0.1.1.1

* Fix error symlink creation on Windows (don't allow invalid characters).

## 0.1.1.0

* Be able to accept multiple `--filter/-f` arguments. A test must match all of them to be run.
* Windows support.

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
