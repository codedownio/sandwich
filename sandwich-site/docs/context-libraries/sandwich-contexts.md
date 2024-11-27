---
id: sandwich-contexts
title: Base contexts
---

The [sandwich-contexts](https://hackage.haskell.org/package/sandwich-contexts) provides a base set of contexts. These contexts are used by other context packages, but are useful in their own right.

## Nix contexts

The [Test.Sandwich.Contexts.Nix](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html) module allows you to introduce a `NixContext`. A `NixContext` essentially represents a snapshot of the [Nixpkgs](https://github.com/NixOS/nixpkgs) package collection, plus some auxiliary stuff like a build cache to keep things speedy.

You can introduce any version of Nixpkgs that you [want](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#t:NixpkgsDerivation). Some are provided in the library, such as [nixpkgsRelease2405](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#v:nixpkgsRelease2405). A basic usage example is as follows:

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-nix/app/Main.hs"
nixDemo :: TopSpec
nixDemo =
  introduceNixContext nixpkgsRelease2405 $
    introduceNixEnvironment ["emacs", "firefox"] $ do
      it "Uses the Nix environment" $ do
        env <- getContext nixEnvironment
        binaries <- listDirectory (env </> "bin")
        info [i|Found binaries in environment: #{binaries}|]
```

## File contexts

The [Test.Sandwich.Contexts.Files](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Files.html) module allows you to introduce named files as contexts.
