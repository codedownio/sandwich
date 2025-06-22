# Changelog for sandwich-webdriver

# 0.3.0.1

* Add more debugging statements to binary fetching.
* Fix getResolutionForDisplay on macOS.
* Pass --user-data-dir to chrome.
* Be able to pass --chrome-no-sandbox.

# 0.3.0.0

* BREAKING CHANGE: switch most monads away from using `MonadBaseControl IO` and switch to `MonadUnliftIO`. We also remove `MonadThrow` constraints, relying only on `MonadIO` for throwing exceptions.
* Fix window positioning commands to use window.devicePixelRatio.
* Add support for introducing Selenium dependencies using Nix with `sandwich-contexts`.
* Improve Haddocks and simplify module structure.
* Export `getDownloadDirectory` accessor for `WebDriver`.
* Be able to obtain dependencies like `ffmpeg` and `Xvfb` on demand.
* Clean up dependencies and fix some warnings on MacOS and Windows.
* Be able to pass a custom Firefox profile in `Capabilities`.
* Remove `hoistExample` helper which didn't belong in this package.
* Support video recording flags `--error-videos`/`--individual-videos`.

# 0.2.3.1

* Binary fetching: don't create the toolsRoot directory unless necessary.
* Binary fetching: use logging instead of stdout/stderr so it doesn't mess up the TUI interface.

# 0.2.3.0

* When autodetecting Chrome, look for `google-chrome-stable` as well as `google-chrome`. (It's found on NixOS.)
* Be able to detect chrome/chromedriver for versions >= 115, with the new Google JSON API.

# 0.2.2.0

* Fix browser path calculation in addCommandLineOptionsToWdOptions.

# 0.2.1.0

* Pass `--headless=new` instead of `--headless` for Chrome >= 110, to address https://www.selenium.dev/blog/2023/headless-is-going-away/.

# 0.2.0.0

* Fix the obtainChromeDriver function now that the zip files contain multiple files. This added a MonadMask constraint to the function so it's a major version bump.

# 0.1.2.0

* Be able to control download directory.
* Add flags to control Selenium paths: `--selenium-jar`, `--chrome-binary`, `--chromedriver-binary`, `--firefox-binary`, `--geckodriver-binary`.

# 0.1.1.0

* Windows support.

# 0.1.0.6

* Remove X11 dependency and replace with per-platform code to get screen resolution.

# 0.1.0.5

* Getting documentation sorted out.
