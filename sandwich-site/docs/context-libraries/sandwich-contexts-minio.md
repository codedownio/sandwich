---
id: sandwich-contexts-minio
title: MinIO
---

The [Test.Sandwich.Contexts.MinIO](https://hackage.haskell.org/package/sandwich-contexts-minio/docs/Test-Sandwich-Contexts-MinIO.html) module provides the ability introduce MinIO S3 servers, either using a raw binary or a container system.

This module is provided as part of the [sandwich-contexts-minio](https://hackage.haskell.org/package/sandwich-contexts-minio) package.

## Via Nix-provided binary

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-minio/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing MinIO" $ do
  introduceNixContext nixpkgsReleaseDefault $ introduceMinIOViaNix defaultMinIOContextOptions $ do
    it "prints the MinIO server info" $ do
      server <- getContext testS3Server
      info [i|Got S3 server: #{server}|]
```

## Via container

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-minio/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing MinIO" $ do
  introduceMinIOViaContainer defaultMinIOContextOptions defaultContainerOptions $ do
    it "prints the MinIO server info" $ do
      server <- getContext testS3Server
      info [i|Got S3 server: #{server}|]
```
