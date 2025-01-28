---
id: sandwich-contexts
title: Base contexts
---

The [sandwich-contexts](https://hackage.haskell.org/package/sandwich-contexts) package provides several useful contexts for adding power to your tests.

This package is mainly concerned with introducing external files and binaries that your tests might need. It also contains some miscellaneous other contexts that don't require any special dependencies.

Other context packages have been split out to address specific needs, such as [sandwich-contexts-kubernetes](./sandwich-contexts-kubernetes) and [sandwich-contexts-minio](./sandwich-contexts-minio).

## Nix contexts

The [Test.Sandwich.Contexts.Nix](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html) module allows you to introduce a [NixContext](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#t:NixContext), which represents a snapshot of the [Nixpkgs](https://github.com/NixOS/nixpkgs) package collection.

You can introduce any version of Nixpkgs that you [want](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#t:NixpkgsDerivation). Several are provided in the library, such as [nixpkgsRelease2405](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#v:nixpkgsRelease2405).

Once you introduce a `NixContext`, you can use it to build Nix artifacts. For example, the [introduceNixEnvironment](https://hackage.haskell.org/package/sandwich-contexts-0.3.0.1/docs/Test-Sandwich-Contexts-Nix.html#v:introduceNixEnvironment) function will allow you to build an environment with a given list of packages.

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

This test will log the various binaries that are available, such as `emacs`, `emacsclient`, and `firefox`. You can now use these artifacts in tests.

## File contexts

The [Test.Sandwich.Contexts.Files](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Files.html) module allows you to introduce named files as contexts. For example, suppose your tests need access to `grep`:

```haskell
tests :: TopSpec
tests =
  introduceFile @"grep" "/path/to/grep" $ do
    it "uses grep for something" $ do
      grep <- askFile @"grep"
      results <- readCreateProcess (proc grep ["foo"]) ""
      ...
```

Notice the syntax here with the `@"grep"`. This is using a combination of type-level strings and `-XTypeApplications` to produce some nice readable syntax.

However, hardcoding a path to a file is not very robust. We also have several function for introducing files from the environment. For example, [introduceBinaryViaEnvironment](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Files.html#v:introduceBinaryViaEnvironment) will search the PATH for a given binary. This node will fail if the binary is not found, making the failure easy to locate (compared to a more inscrutable error deep in your tests).

```haskell
tests :: TopSpec
tests =
  introduceBinaryViaEnvironment @"grep" $ do
    it "uses grep for something" $ do
      grep <- askFile @"grep"
      ...
```

It's usually a good idea to decouple the "introduce" node from what the tests need. For this reason we provide the [HasFile](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Files.html#t:HasFile) alias, which you can use like this:

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-files/app/Main.hs"
filesDemo :: TopSpec
filesDemo =
  introduceBinaryViaEnvironment @"grep" $ do
    testsWithGrep

testsWithGrep :: (HasFile context "grep") => SpecFree context IO ()
testsWithGrep = do
  it "Uses grep binary" $ do
    grep <- askFile @"grep"
    output <- readCreateProcess (proc grep ["--version"]) ""
    info [i|grep --version output: #{output}|]
```

Now we can wrap `testsWithGrep` in any "introduce" node that provides the file! Including more fancy ones, like the Nix-based ones below.

## File + Nix contexts = ❤️

Where Nix context and file contexts really shine is when you use them together. For example, the [introduceBinaryViaNixPackage](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Files.html#v:introduceBinaryViaNixPackage) function depends on a [NixContext](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-Nix.html#t:NixContext) being available, and uses it to introduce a given binary from a given package.

You can see that put together here, where we introduce the `hello` binary from the `hello` package.

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-nix-binary/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing a Nix binary" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"hello" "hello" $ do
      it "uses the hello binary" $ do
        useHello

useHello :: (MonadIO m, MonadReader context m, HasFile context "hello") => m ()
useHello = do
  helloPath <- askFile @"hello"
  readCreateProcess (proc helloPath []) "" >>= (`shouldBe` "Hello, world!\n")
```

This brings together a robust solution for using external binaries:
* It is fully reproducible, as it uses a pinned Nixpkgs checkout.
* The test function `useHello` is decoupled from the introduction method, via the `HasFile context "hello"` constraint.

By the way, you can browse all the available Nix packages at [search.nixos.org](https://search.nixos.org/packages).
