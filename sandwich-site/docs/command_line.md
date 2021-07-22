---
id: command_line
title: Command line arguments
sidebar_label: Command line
---


If you use [runSandwichWithCommandLineArgs](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs), then you can control your tests with command line arguments. This section contains a quick overview of the available arguments, and explains how you can add your own custom ones.

## Quick reference

* Choose a formatter: `--tui`, `--print`, `--print-failures`, `--silent`, `--auto`.
* Choose a default log level: `--debug`, `--info`, `--warn`, `--error`.
* Repeat the test suite N times: `--repeat n` (useful to exercise flaky tests)
* Filter the test tree to a string: `--filter some_string`.
* Learn about extra flags controlling extensions: `--print-slack-flags`, `--print-webdriver-flags`.
* List test modules and flags to run them individually: `--list-tests`. (Requires the use of [test discovery](discovery).)

This list isn't exhaustive; to learn more about shorthands and other options pass `--help`.

## Custom command line arguments

For more complex tests, you may want to be able to control them with custom command line arguments. To do this, you can write your own set of options with [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) and run your tests with [runSandwichWithCommandLineArgs'](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:runSandwichWithCommandLineArgs-39-). The primed version accepts your custom options parser.

This will cause your options to be parsed along with the default ones. To obtain the full command line options inside a test, call [getCommandLineOptions](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Contexts.html#v:getCommandLineOptions). This will return a [CommandLineOptions a](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Misc.html#t:CommandLineOptions), where `a` is your custom options type. A convenient helper is [getUserCommandLineOptions](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Contexts.html#v:getCommandLineOptions), which returns only the `a` without the built-in stuff.

The example below shows this in action. Note that the type of the top-level spec changes from the usual [TopSpec](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Misc.html#t:TopSpec) to [TopSpecWithOptions' a](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Misc.html#t:TopSpecWithOptions-39-).

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-command-line-args/app/Main.hs"
import Options.Applicative
import Test.Sandwich

data MyArgs = MyArgs {
  myArgsFoo :: String
  , myArgsBar :: Int
  }

myArgsParser :: Parser MyArgs
myArgsParser = MyArgs
  <$> strOption (long "foo" <> help "Value of foo" <> metavar "STRING")
  <*> option auto (long "bar" <> showDefault <> help "Value of bar" <> value 1 <> metavar "INT")

commandLineArgsDemo :: TopSpecWithOptions' MyArgs
commandLineArgsDemo = describe "Custom command line args" $ do
  it "Uses the custom command line args" $ do
    MyArgs {..} <- getUserCommandLineOptions
    warn [i|Got foo: #{myArgsFoo}|]
    warn [i|Got bar: #{myArgsBar}|]

main :: IO ()
main = runSandwichWithCommandLineArgs' testOptions myArgsParser commandLineArgsDemo
```

Now we can run this with

```
cd sandwich/sandwich-demos
stack run demo-command-line-args -- --print --foo asdf --bar 42
```

and the tests will use our custom values.
