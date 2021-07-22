---
id: sandwich-quickcheck
title: QuickCheck integration
---

You can use `sandwich-quickcheck` (provided as a separate package) to easily integrate integrate [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) tests into the test tree.

Haddocks can be found [here](http://hackage.haskell.org/package/sandwich-quickcheck/docs/Test-Sandwich-QuickCheck.html).

## Usage

To use `sandwich-quickcheck`, just add the package to your project. Then, introduce a QuickCheck argument context using [introduceQuickCheck](http://hackage.haskell.org/package/sandwich-quickcheck/docs/Test-Sandwich-QuickCheck.html#v:introduceQuickCheck). Now you can start writing props as test nodes using the [prop](http://hackage.haskell.org/package/sandwich-quickcheck/docs/Test-Sandwich-QuickCheck.html#v:prop) function. For example:

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-quickcheck/app/Main.hs"
quickCheckDemo :: TopSpec
quickCheckDemo = describe "QuickCheck tests" $ introduceQuickCheck $ do
  prop "List reversal" $ \(xs :: [Int]) -> reverse (reverse xs) == xs
  prop "Failing list reversal" $ \(xs :: [Int]) -> (reverse xs) == xs
```

## Modifying the args

If you use [introduceQuickCheck'](http://hackage.haskell.org/package/sandwich-quickcheck/docs/Test-Sandwich-QuickCheck.html#v:introduceQuickCheck'), you can pass your own value for the QuickCheck [Args](https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#t:Args). The default version uses [stdArgs](https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#v:stdArgs).

If you want to modify the already-introduced arguments in a test tree, we provide the [modifyArgs](http://hackage.haskell.org/package/sandwich-quickcheck/docs/Test-Sandwich-QuickCheck.html#v:modifyArgs) function, as well as helpers like `modifyMaxSize`, `modifyMaxDiscardRatio`, etc. These are modelled directly after HSpec's [Test.Hspec.QuickCheck](https://hackage.haskell.org/package/hspec/docs/Test-Hspec-QuickCheck.html).

## Controlling QuickCheck parameters with command line args

There are not (yet) any built-in command line arguments for controlling QuickCheck parameters such as `maxSize`. However, you can add [custom command line options](../command_line) to control any parameters you like.
