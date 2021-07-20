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
  it "opens Google and searches" $ withSession1 $ do
    openPage "http://www.google.com"
    search <- findElem (ByCSS "input[title='Search']")
    click search
    sendKeys "asdf\n" search

main :: IO ()
main = runSandwich defaultOptions spec
```

To see a demo, try running `stack run demo-webdriver` in the Sandwich repo.

## Browser sessions

You can start a Selenium session using the `withSession` function. It accepts a string key representing the name of the session. Each time a new session name is seen, it will be created if it doesn't already exist.

The library provides `withSession1`/`withSession2` as convenience functions for `withSession "browser1"`/`withSession "browser2"`, but you can use your own keys if you need.

For example, the code below opens two windows with a different site in each.

```haskell
spec :: TopSpec
spec = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "two browser sessions" $ do
    it "opens Google" $ withSession1 $ openPage "http://www.google.com"
    it "opens Yahoo" $ withSession2 $ openPage "http://www.yahoo.com"
```
## Window positioning

You can use the functions in [Test.Sandwich.WebDriver.Windows](http://hackage.haskell.org/package/sandwich-webdriver/docs/Test-Sandwich-WebDriver-Windows.html) to arrange browser windows on the screen. This is useful when you want to watch two browsers simultaneously accessing a collaborative app.

The code below extends the previous example with window positioning. You can find this in the `webdriver-positioning` demo.

```haskell
positioning :: TopSpec
positioning = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "two windows side by side" $ do
    it "opens Google" $ withSession1 $ do
      openPage "http://www.google.com"
      setWindowLeftSide

    it "opens Google" $ withSession2 $ do
      openPage "http://www.yahoo.com"
      setWindowRightSide
```

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

Alternatively, if you use Sandwich's [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs) in conjunction with [introduceWebDriverOptions](http://hackage.haskell.org/package/sandwich-webdriver/docs/Test-Sandwich-WebDriver.html#v:introduceWebDriverOptions), you can enable headless mode by passing `--headless`.

### Xvfb

Xvfb can be used to run your browser on a separate, "virtual" X11 display, different from the one connected to your monitor. This was more useful before headless browser modes existed, but it's still important because it gives you the ability to record **video**. When a Selenium test is running on an Xvfb display, you can use [ffmpeg](https://ffmpeg.org/) to record videos of the test runs for later examination.

Xvfb mode can be configured manually just like headless mode.

```haskell
wdOptions = (defaultWdOptions "/tmp/tools") {
  capabilities = chromeCapabilities
  , runMode = RunInXvfb XvfbConfig
  }
```

Or, if you use Sandwich's [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs) in conjunction with [introduceWebDriverOptions](http://hackage.haskell.org/package/sandwich-webdriver/docs/Test-Sandwich-WebDriver.html#v:introduceWebDriverOptions), you can enable Xvfb mode by passing `--xvfb`.

:::note
Xvfb and ffmpeg must be installed in the test environment to use these features.
:::

## Recording videos

As discussed above, recording video doesn't work in headless (`--headless`) mode. It requires normal (`--current`) or Xvfb (`--xvfb`) mode.

### Manually

Using the methods in [Test.Sandwich.WebDriver.Video](http://hackage.haskell.org/package/sandwich-webdriver/docs/Test-Sandwich-WebDriver-Video.html), you can wrap arbitrary sections of a test in video recording. The example below can be found in the `webdriver-video` demo.

```haskell
manualVideo :: TopSpec
manualVideo = introduceWebDriver (defaultWdOptions "/tmp/tools") $ do
  describe "video recording" $ do
    it "opens Google" $ withSession1 $ do
      openPage "http://www.google.com"

      Just dir <- getCurrentFolder
      let path = dir </> "video" -- No extension needed
      bracket (startBrowserVideoRecording path defaultVideoSettings) endVideoRecording $ \_ -> do
        search <- findElem (ByCSS [i|input[title="Search"]|])
        click search
        sendKeys "Haskell Sandwich" search
        findElem (ByCSS [i|input[type="submit"]|]) >>= click
```

You can also wrap video around multiple tests by using the [around](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:around) node, or around individual tests using [aroundEach](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:aroundEach), etc.

### With command line options

If you use Sandwich's [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs) in conjunction with [introduceWebDriverOptions](http://hackage.haskell.org/package/sandwich-webdriver/docs/Test-Sandwich-WebDriver.html#v:introduceWebDriverOptions), then you can take advantage of the built-in command line arguments `--individual-videos` and `--error-videos`. The former will record videos of every individual test and store them in the corresponding folder on disk. The latter will do the same, but will delete the videos if the test ran successfully, so you only end up with error videos.

You can try this out by running:

```bash
stack run demo-webdriver -- --individual-videos
```

## Screenshots

Because every Sandwich test tree has an associated directory in the filesystem, it's easy to capture screenshots during a test.

```haskell
Just dir <- getCurrentFolder
screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")
```

## Custom Selenium and driver binaries

By default, the WebDriver machinery will obtain the Selenium JAR and driver binaries from the web automatically. However, you can also manually configure which ones are used by passing a disk path.

```haskell
data WdOptions = WdOptions {
  ...
  , seleniumToUse = UseSeleniumAt "/path/to/selenium.jar"
  , chromeDriverToUse = UseChromeDriverAt "/path/to/chromedriver"
  , geckoDriverToUse = UseGeckoDriverAt "/path/to/geckodriver"
}
```

Alternatively, you can pass [DownloadSeleniumFrom](http://hackage.haskell.org/package/sandwich-webdriver/docs/Test-Sandwich-WebDriver-Config.html#t:SeleniumToUse) with a URL to download, and similarly for the other options. Please see the Haddocks for more details.

## Running tests in parallel with a webdriver pool

If you have a lot of test files, you probably want to run them in parallel so the test suite finishes faster. However, you probably don't want to run them **all** in parallel, since running that many browser sessions could bog down your CI machine. A good solution is to use [Data.Pool](https://hackage.haskell.org/package/resource-pool) to introduce a fixed-size pool of reusable WebDriver contexts.

You can follow along with this example in the `webdriver-pool` demo.

The first thing we need to do is come up with a label and introduce a pool of the desired size.

```haskell
webDriverPool = Label :: Label "webDriverPool" (Pool WebDriver)
type HasWebDriverPool context = HasLabel context "webDriverPool" (Pool WebDriver)

introduceWebDriverPool poolSize wdOptions' =
  introduce "Introduce webdriver pool" webDriverPool alloc cleanup
  where
    alloc = do
      wdOptions <- addCommandLineOptionsToWdOptions
                   <$> (getCommandLineOptions @())
                   <*> pure wdOptions'
      runRoot <- fromMaybe "/tmp" <$> getRunRoot
      liftIO $ createPool
        (allocateWebDriver' runRoot wdOptions) cleanupWebDriver' 1 30 poolSize
    cleanup = liftIO . destroyAllResources
```

There's some plumbing here because we want to pipe the command line options through. The important part is at the end, where we make a pool that knows how to allocate and deallocate WebDrivers using the lower-level `allocateWebDriver'` and `cleanupWebDriver'` functions.

Next, we make another introduce node to *claim* a WebDriver from the pool.

```haskell
claimWebdriver spec = introduceWith' (
  defaultNodeOptions {nodeOptionsRecordTime=False, nodeOptionsCreateFolder=False}
  ) "Claim webdriver" webdriver wrappedAction spec
  where
    wrappedAction action = do
      pool <- getContext webDriverPool
      withResource pool $ \sess ->
        (void $ action sess) `finally` closeAllSessions sess
```

There's also some plumbing here where we tweak the node options of this to play better with [test timing](../timing). This code obtains the pool context, then claims a WebDriver to pass to its sub-nodes. It also cleans up at the end by calling `closeAllSessions`.

Having written these functions, we can finally write our tests. The following will run all the tests, up to four at a time, re-using the WebDrivers among them.

```haskell
tests :: TopSpecWithOptions
tests =
  introduceWebDriverPool 4 (defaultWdOptions "/tmp/tools") $
    parallel $
      replicateM_ 20 $
        claimWebdriver $
          it "opens Google" $ withSession1 $
            openPage "http://www.google.com"
```

Of course, in real use you probably want to introduce different tests to run in parallel. You can use [Test Discovery](../discovery) to automatically generate them.
