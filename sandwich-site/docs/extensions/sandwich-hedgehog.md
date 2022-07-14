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

## Demo

A demo is provided in the Sandwich repo.

```shell
git clone git@github.com:codedownio/sandwich.git
cd sandwich
stack run demo-hedgehog -- --tui
```

<!-- ## Modifying the args -->

<!-- If you use [introduceHedgehog'](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html#v:introduceHedgehog'), you can pass your own value for the Hedgehog [Args](https://hackage.haskell.org/package/Hedgehog/docs/Test-Hedgehog.html#t:Args). The default version uses [stdArgs](https://hackage.haskell.org/package/Hedgehog/docs/Test-Hedgehog.html#v:stdArgs). -->

<!-- If you want to modify the already-introduced arguments in a test tree, we provide the [modifyArgs](http://hackage.haskell.org/package/sandwich-hedgehog/docs/Test-Sandwich-Hedgehog.html#v:modifyArgs) function, as well as helpers like `modifyMaxSize`, `modifyMaxDiscardRatio`, etc. These are modelled directly after HSpec's [Test.Hspec.Hedgehog](https://hackage.haskell.org/package/hspec/docs/Test-Hspec-Hedgehog.html). -->

<!-- ## Controlling Hedgehog parameters with command line args -->

<!-- There are not (yet) any built-in command line arguments for controlling Hedgehog parameters such as `maxSize`. However, you can add [custom command line options](../command_line) to control any parameters you like. -->
