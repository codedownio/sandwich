---
id: sandwich-contexts
title: Base contexts
---

The [sandwich-contexts](https://hackage.haskell.org/package/sandwich-contexts) provides a base set of contexts. These contexts are used by other context packages, but are useful in their own right.

## Nix contexts

The [Test.Sandwich.Contexts.Nix](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html) module allows you to introduce a `NixContext`. A `NixContext` essentially represents a snapshot of the [Nixpkgs](https://github.com/NixOS/nixpkgs) package collection, plus some auxiliary stuff like a build cache to keep things speedy.

You can introduce any version of Nixpkgs that you [want](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#t:NixpkgsDerivation). Some are provided in the library, such as [nixpkgsRelease2405](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#v:nixpkgsRelease2405). Once you introduce a `NixContext`, you can use it to build Nix artifacts. For example, the [introduceNixEnvironment](https://hackage.haskell.org/package/sandwich-contexts-0.3.0.1/docs/Test-Sandwich-Contexts-Nix.html#v:introduceNixEnvironment) function will allow you to build an environment with a given list of packages.

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-nix/app/Main.hs"p
nixDemo :: TopSpec
nixDemo =
  introduceNixContext nixpkgsRelease2405 $
    introduceNixEnvironment ["emacs", "firefox"] $ do
      it "Uses the Nix environment" $ do
        env <- getContext nixEnvironment
        binaries <- listDirectory (env </> "bin")
        info [i|Found binaries in environment: #{binaries}|]
```

This test will log the various binaries that are available, such as `emacs`, `emacsclient`, and `firefox`. You can now use these artifacts in tests.

## File contexts

The [Test.Sandwich.Contexts.Files](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Files.html) module allows you to introduce named files as contexts.
