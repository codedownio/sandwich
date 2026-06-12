# Changelog for sandwich-contexts-kubernetes

## Unreleased

* Add `Test.Sandwich.Contexts.Kubernetes.MetricsServer` to install the metrics-server (so `kubectl top` / the metrics API work), with configurable manifest source and `--kubelet-insecure-tls`.
* Add `Test.Sandwich.Contexts.Kubernetes.OOMWatcher` to detect OOMKilled containers, via a streaming pod watch (`withOOMWatcher`) or a one-shot check (`checkForOOMKills`).
* Add `Test.Sandwich.Contexts.Kubernetes.ResourceWatcher` to record per-pod CPU and memory over a test using `kubectl top`, writing a CSV, peak summaries, and SVG charts. All three watchers default to cluster-wide and can be scoped to a namespace.
* Make metrics-server installation on kind clusters configurable via `kindClusterMetricsServer` (defaults on, bumped to v0.7.2) instead of always installing v0.6.4.
* Clean up leftover container-runtime volumes and per-profile state dirs after tearing down a Minikube cluster.

## 0.1.3.0

* Add `Test.Sandwich.Contexts.Kubernetes.PostgresServer`.
* Add `Test.Sandwich.Contexts.Kubernetes.Typesense`.
* Support reading the image name from OCI images.
* Be able to install the MinIO operator from Nix.
* Be able to install Kata Containers from a Helm chart, and drop the legacy KataContainers method.
* Export `KustomizationDir` from `Test.Sandwich.Contexts.Kubernetes.MinioS3Server`.
* Use the latest `sandwich` process file logging and managed asyncs.

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
