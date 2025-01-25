---
id: sandwich-contexts-postgresql
title: PostgreSQL
---

The [Test.Sandwich.Contexts.PostgreSQL](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-PostgreSQL.html) module provides tools for introducing PostgreSQL databases, either via a container (Docker or Podman) or via a raw process (typically obtaining the binary from Nix).

The container method is traditional, but the raw method can be nice because it tends to leave less junk on the system such as container images, networks, and volumes.

This module is currently included in [sandwich-contexts](./sandwich-contexts), but may be split into its own package in the future.

## Raw binary via Nix

Here's an example of using a PostgreSQL server using a [NixContext](./sandwich-contexts#nix-contexts) and [introducePostgresViaNix](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-PostgreSQL.html#v:introducePostgresViaNix).

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-postgres/app/Main.hs"
import Database.PostgreSQL.Simple (Only(..), connectPostgreSQL, query_)
import Test.Sandwich.Contexts.PostgreSQL

spec :: TopSpec
spec = describe "Introducing PostgreSQL via Nix" $ do
  introduceNixContext nixpkgsReleaseDefault $ introducePostgresViaNix defaultPostgresNixOptions $ do
    it "runs a simple query" $ do
      PostgresContext {..} <- getContext postgres
      conn <- connectPostgreSQL connString
      [Only n] <- query_ conn "select 2 + 2"
      n `shouldBe` 4
```

## Container

Here's an example of using a container via [introducePostgresViaContainer](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-PostgreSQL.html#v:introducePostgresViaContainer). You can configure the container options using [PostgresContainerOptions](https://hackage.haskell.org/package/sandwich-contexts/docs/Test-Sandwich-Contexts-PostgreSQL.html#t:PostgresContainerOptions).

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-postgres/app/Main.hs"
import Database.PostgreSQL.Simple (Only(..), connectPostgreSQL, query_)
import Test.Sandwich.Contexts.PostgreSQL

spec :: TopSpec
spec = describe "Introducing PostgreSQL via container" $ do
  introducePostgresViaContainer defaultPostgresContainerOptions $ do
    it "runs a simple query" $ do
      PostgresContext {..} <- getContext postgres
      conn <- connectPostgreSQL connString
      [Only n] <- query_ conn "select 2 + 2"
      n `shouldBe` 4
```


## About the use of Unix sockets

Starting a Postgres process on a randomly chosen port is tricky, because Postgres currently lacks a setting for choosing its own port and reporting it back to us. So, the only way to start it on a random TCP port is to first manually find a free port on the system and then start Postgres with it. Since this procedure is inherently racy, it can cause failures if your tests are starting lots of Postgres instances (or other network-using processes) in parallel.

This module takes a different approach: it starts the Postgres instance on a Unix socket, which can never fail. You can connect to it via the Unix socket directly if you like. If you use the TCP-based methods like `introducePostgresViaNix`, they will open a TCP socket inside the test process and then run a proxy to forward packets to the Postgres server's Unix socket.
