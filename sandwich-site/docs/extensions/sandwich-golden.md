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

The first time you run this test, it will generate the directory `<project root>/.golden`, containing the desired outputs. You can then check this into version control.

:::tip

You can change the Golden directory from the default by passing `--golden-dir=/path/to/golden/dir`.

:::

Now suppose you're working on your project, and you inadvertently changed the implementation of `myStringFunc` to this:

```haskell
myStringFunc _ = "bar"
```

If you run the tests again, this will fail, saying `myStringFunc` is expected to return "foo" but returned "bar". This is good; the test is enforcing that this value stays golden.

Once you've run some failing tests, they will also fill in "actual" files alongside the "golden" files in the `.golden` dir. You can compare these files to see what changed in the current code.

### Updating golden files

Now suppose we want to update the golden files, so that "bar" becomes the desired output for `myStringFunc`.

Simply run the tests with the `--golden-update` flag. This will print the files it updates:

```shell
Replacing golden with actual...
  .golden/myStringFunc/golden <-- .golden/myStringFunc/actual
Done!
```

Now you can check the updated golden files into the repo.

## Demo

A demo is provided in the Sandwich repo.

```shell
git clone git@github.com:codedownio/sandwich.git
cd sandwich
stack run demo-golden -- --print
```
