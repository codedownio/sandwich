---
id: sandwich-webdriver
title: Selenium integration
---

Setting up Selenium tests normally requires manual work to obtain the Selenium server JAR file and a driver program for your browser, launch the server, and connect to the server with your client library. It can be error-prone to make sure the versions are compatible and everything runs smoothly.

The `sandwich-webdriver` extension streamlines this by automatically downloading the latest compatible binary files and introducing the contexts you need to use the [webdriver](https://hackage.haskell.org/package/webdriver) package within Sandwich. Here's how easy it is to get started:

```haskell
import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.WebDriver

spec :: TopSpec
spec = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  it "opens Google and searches" $ withBrowser1 $ do
    openPage "http://www.google.com"
    search <- findElem (ByCSS "input[title='Search']")
    click search
    sendKeys "asdf\n" search

main :: IO ()
main = runSandwich defaultOptions spec
```

Check out the [WdOptions](#) and [RunMode](#) options for more on what this package can do, or look at the sections below.

## Launching browsers in the background

This package makes it easy to run Selenium tests in the background, using either [Xvfb](https://en.wikipedia.org/wiki/Xvfb) or the headless mode of your browser.

### Headless

Many browsers now have the ability to natively run in headless mode. For example, passing these modified `WdOptions` to `introduceWebDriver` will run using headless Firefox.

```haskell
wdOptions = (defaultWdOptions "/tmp/tools") {
  capabilities = firefoxCapabilities
  , runMode = RunHeadless defaultHeadlessConfig
  }
```

### Xvfb

Xvfb can be used to run your browser on a separate, "virtual" X11 display, different from the one connected to your monitor. This was more useful before headless browser modes existed, but it's still important because it gives you the ability to record **video**.

When a Selenium test is running on an Xvfb display, you can use [ffmpeg](https://ffmpeg.org/) to record videos of the test runs for later examination.

```haskell
TODO: example
```

## Multiple browser sessions

You can open multiple Selenium sessions using the `withBrowser` function. It accepts a string key representing the name of the session. Each time a new session name is seen, it will be created if it doesn't already exist.

The library provides `withBrowser1`/`withBrowser2` as convenience functions for `withBrowser "browser1"`/`withBrowser "browser2"`, but you can use your own keys if you need.

For example, the code below open two windows, positions them on the left and right side of the screen respectively, and opens a different site in each.

```haskell
spec :: TopSpec
spec = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "two windows side by side" $ do
    it "opens Google" $ withBrowser1 $ openPage "http://www.google.com"
    it "opens Yahoo" $ withBrowser2 $ openPage "http://www.yahoo.com"
```

## Window positioning

You can use the functions in [Test.Sandwich.WebDriver.Windows](#) to arrange browser windows on the screen. This is useful when you want to watch two browsers simultaneously accessing a collaborative app.

The code below extends the previous example with window positioning.

```haskell
spec :: TopSpec
spec = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "two windows side by side" $ do
    it "opens Google" $ withBrowser "browser1" $ do
      setWindowLeftSide
      openPage "http://www.google.com"

    it "opens Google" $ withBrowser "browser2" $ do
      setWindowRightSide
      openPage "http://www.yahoo.com"
```

## Screenshots

Because every Sandwich test tree has an associated directory in the filesystem, it's easy to capture screenshots during a test.

```haskell
TODO
```

## Custom Selenium and driver binaries

TODO

## Running tests in parallel with a webdriver pool

TODO
