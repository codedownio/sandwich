# Changelog for sandwich-contexts-kubernetes

## 0.1.2.0

* Add `getKubectlEnvironment`, similar to `askKubectlEnvironment` but not requiring reader context.
* Re-export `NetworkAddress` from `Test.Sandwich.Contexts.Kubernetes.MinioS3Server`.
* Add more low-level functions to `Test.Sandwich.Contexts.Kubernetes.Namespace`.

## 0.1.1.1

* Bump Kata context to use latest 3.19.1 by default.
* Add debugging to KataContainers.hs.
* Fix kubectlBinary reference.

## 0.1.1.0

* Switch to `kubernetes-api` and `kubernetes-api-client`.
* Fix compatibility with text-2.1.2.
* Be able to configure network policies for MinioS3Server.hs.
* Fix MinioS3Server.hs destroy.
* Remove a bunch of Control.Monad.Catch usage.

## 0.1.0.0

* Initial release
