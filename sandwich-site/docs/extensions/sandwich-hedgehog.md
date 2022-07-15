---
id: sandwich-hedgehog
title: Hedgehog integration
---

You can use `sandwich-hedgehog` (provided as a separate package) to easily integrate integrate [Hedgehog](https://hedgehog.qa/) tests into the test tree.

Haddocks can be found [here](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html).

## Usage

To use `sandwich-hedgehog`, just add the package to your project. Then, introduce a Hedgehog argument context using [introduceHedgehog](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html#v:introduceHedgehog). Now you can start writing props as test nodes using the [prop](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html#v:prop) function. For example:

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-hedgehog/app/Main.hs"
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

hedgehogDemo :: TopSpec
hedgehogDemo = describe "Hedgehog tests" $ introduceHedgehog $ do
  prop "List reversal" $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs

  prop "Failing list reversal" $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse xs === xs
```

## Modifying the parameters

If you use [introduceHedgehog'](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html#v:introduceHedgehog'), you can pass your own value for the [HedgehogParameters](https://hackage.haskell.org/package/Hedgehog/docs/Test-Hedgehog.html#t:HedgehogParameters).

If you want to modify the already-introduced arguments in a test tree, we provide the [modifyArgs](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html#v:modifyArgs) function, as well as helpers like `modifySeed`, `modifySize`, etc. These are modelled directly after HSpec's [Test.Hspec.Hedgehog](https://hackage.haskell.org/package/hspec/docs/Test-Hspec-Hedgehog.html).

## Controlling Hedgehog parameters with command line args

Some Hedgehog parameters can be controlled via the command line. To see the valid options, run Sandwich with `--print-hedgehog-flags`.

```shell
Usage: demo [--hedgehog-seed STRING] [--hedgehog-size INT]
            [--hedgehog-discard-limit INT]
            [--hedgehog-shrink-limit INT]
            [--hedgehog-shrink-retries INT] [--hedgehog-confidence INT]

Available options:
  --hedgehog-seed STRING   Seed as a tuple (a, b)
  --hedgehog-size INT      Size of the randomly-generated data
  --hedgehog-discard-limit INT
                           The number of times a property is allowed to discard
                           before the test runner gives up
  --hedgehog-shrink-limit INT
                           The number of times a property is allowed to shrink
                           before the test runner gives up and prints the
                           counterexample
  --hedgehog-shrink-retries INT
                           The number of times to re-run a test during shrinking
  --hedgehog-confidence INT
                           The acceptable occurrence of false positives
```

## Demo

A demo is provided in the Sandwich repo.

```shell
git clone git@github.com:codedownio/sandwich.git
cd sandwich
stack run demo-hedgehog -- --tui
```
