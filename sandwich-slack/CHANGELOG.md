# Changelog for sandwich-slack

## 0.2.0.0

* BREAKING CHANGE: switch most monads away from using `MonadBaseControl IO` and switch to `MonadUnliftIO`.
* Windows support improvements.

## 0.1.2.0

* GHC 9.6 support

## 0.1.1.0

* Windows support.
* Export SlackFormatter type

## 0.1.0.6

* Relax base bound so we can build on GHC 9
