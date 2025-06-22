# Changelog for sandwich-contexts

## 0.3.0.3

* Use --no-link in Nix builds to avoid unnecessary result symlinks.
* Make withProxyToUnixSocket more robust with a timeout.
* Add nixpkgsRelease2411, nixpkgsRelease2505, nixpkgsMaster.
* Export findFirstFileInDirs, and make tryFindBinary search under Applications on macOS.
* Add buildNixPackage functions + remove a bunch of Control.Monad.Catch usage.

## 0.3.0.2

* Add `postgresNixConfExtraLines` to `PostgresNixOptions`, to be able to add extra lines to `postgresql.conf`.
* Add 'fakeSmtpServerHostname' to FakeSmtpServer.
* Add some generic options for launching containers.
* Enable Unix socket based contexts on Windows.
* Add withPostgresViaNix', withPostgresUnixSocketViaNix'.
* Add makeNixContext, makeNixContext'.

## 0.3.0.1

* Automatically find sufficiently short Unix socket paths when the system temp ones are too long.

## 0.3.0.0

* Initial release.
