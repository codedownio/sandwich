# Changelog for sandwich-webdriver

# Unreleased

# 0.2.2.0

* Fix browser path calculation in addCommandLineOptionsToWdOptions

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
