---
id: sandwich-golden
title: Golden testing
---

Sandwich has built-in support for golden testing. Golden testing is a form of unit testing where the desired output is stored in separate file(s), usually checked into your repo. When the tests are run, they invoke your code and compare the output to the file version. A good golden testing framework allows you to easily generate (and re-generate!) the output files.

Haddocks can be found [here](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Golden.html).

## Usage

The main function for working with golden tests is [golden](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich-Golden.html#v:golden). It's a simple wrapper around [it](http://hackage.haskell.org/package/sandwich/docs/Test-Sandwich.html#v:it) with some options for the test name and how to read/write the golden file. Using it looks like this:

```haskell
import Test.Sandwich
import Test.Sandwich.Golden

goldenDemo :: TopSpec
goldenDemo = describe "Simple tests" $ do
  describe "myStringFunc" $
    golden $ goldenString "myStringFunc" (myStringFunc ())

myStringFunc _ = "foo"
```

The first time you run this test, it will

### Updating golden files

TODO

## Demo

A demo is provided in the Sandwich repo.

```shell
git clone git@github.com:codedownio/sandwich.git
cd sandwich
stack run demo-golden -- --tui
```
