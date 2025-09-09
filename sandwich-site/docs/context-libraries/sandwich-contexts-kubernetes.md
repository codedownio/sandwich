---
id: sandwich-contexts-kubernetes
title: Kubernetes
---

The [sandwich-contexts-kubernetes](https://hackage.haskell.org/package/sandwich-contexts-kubernetes) package provides contexts for creating and managing Kubernetes clusters in your tests, along with utilities for interacting with them.

This package supports creating clusters using either [kind](https://kind.sigs.k8s.io/) or [Minikube](https://minikube.sigs.k8s.io), with binaries obtained either from the system PATH or from Nix. It also includes functions for waiting for pods and services, running kubectl commands, service forwarding, port forwarding, and image management.

## Creating clusters

### Kind clusters

[Kind](https://kind.sigs.k8s.io/) is a tool for running local Kubernetes clusters using Docker container "nodes."

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-kubernetes-kind/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceKindClusterViaNix defaultKindClusterOptions $ do
      it "prints the cluster info" $ do
        kcc <- getContext kubernetesCluster
        info [i|Got Kubernetes cluster context: #{kcc}|]
```

You can also obtain the `kind` binary from your system PATH:

```haskell
spec :: TopSpec
spec = describe "Kind cluster from environment" $
  introduceBinaryViaEnvironment @"kind" $
    introduceBinaryViaEnvironment @"kubectl" $
      introduceKindClusterViaEnvironment defaultKindClusterOptions $ do
        -- Your tests here
```

Kind clusters can be configured using [KindClusterOptions](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#t:KindClusterOptions):

- **Number of nodes**: Set via [`kindClusterNumNodes`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#v:kindClusterNumNodes)
- **Extra flags**: Pass additional kind flags via [`kindClusterExtraFlags`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#v:kindClusterExtraFlags)
- **Container labels**: Apply labels to created containers via [`kindClusterContainerLabels`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#v:kindClusterContainerLabels)
- **Port mappings**: Expose cluster ports to the host via [`kindClusterExtraPortMappings`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#v:kindClusterExtraPortMappings)
- **Mounts**: Mount host directories into cluster nodes via [`kindClusterExtraMounts`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#v:kindClusterExtraMounts)
- **Name prefix**: Set cluster name prefix via [`kindClusterNamePrefix`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-KindCluster.html#v:kindClusterNamePrefix)

### Minikube clusters

[Minikube](https://minikube.sigs.k8s.io) runs a single-node Kubernetes cluster locally, supporting various container runtimes.

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-kubernetes-minikube/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster via Minikube" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $ do
      it "prints the cluster info" $ do
        kcc <- getContext kubernetesCluster
        info [i|Got Kubernetes cluster context: #{kcc}|]
```

Similarly, you can use binaries from your environment:

```haskell
spec :: TopSpec
spec = describe "Minikube cluster from environment" $
  introduceBinaryViaEnvironment @"minikube" $
    introduceBinaryViaEnvironment @"kubectl" $
      introduceMinikubeClusterViaEnvironment defaultMinikubeClusterOptions $ do
        -- Your tests here
```

Minikube clusters can be configured using [MinikubeClusterOptions](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#t:MinikubeClusterOptions):

- **Number of nodes**: Set via [`minikubeClusterNumNodes`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#v:minikubeClusterNumNodes)
- **Extra flags**: Pass additional minikube flags via [`minikubeClusterExtraFlags`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#v:minikubeClusterExtraFlags)
- **Name prefix**: Set cluster name prefix via [`minikubeClusterNamePrefix`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#v:minikubeClusterNamePrefix)
- **Driver**: Choose container runtime via [`minikubeClusterDriver`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#v:minikubeClusterDriver) (docker, podman, etc.)
- **CPUs**: Set CPU allocation via [`minikubeClusterCpus`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#v:minikubeClusterCpus)
- **Memory**: Set memory allocation via [`minikubeClusterMemory`](https://hackage.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinikubeCluster.html#v:minikubeClusterMemory)

## Working with clusters

Once you have a cluster, you can access its context:

```haskell
it "uses cluster context" $ do
  kcc <- getContext kubernetesCluster
  info [i|Cluster name: #{kubernetesClusterName kcc}|]
  info [i|Kubeconfig path: #{kubernetesClusterKubeConfigPath kcc}|]
  info [i|Number of nodes: #{kubernetesClusterNumNodes kcc}|]
```

### Running kubectl commands

The library provides utilities for running kubectl commands with the correct configuration:

```haskell
it "runs kubectl commands" $ do
  (kubectlBinary, env) <- askKubectlArgs
  pods <- readCreateProcessWithLogging
    ((proc kubectlBinary ["get", "pods", "-A"]) { env = Just env }) ""
  info [i|Cluster pods: #{pods}|]
```

### Waiting for resources

The library includes utilities for waiting for Kubernetes resources to be ready:

```haskell
it "waits for resources" $ do
  -- Wait for pods to exist
  waitForPodsToExist "default" (LabelSelector [("app", "my-app")])

  -- Wait for pods to be ready
  waitForPodsToBeReady "default" (LabelSelector [("app", "my-app")])

  -- Wait for service endpoints
  waitForServiceEndpointsToExist "default" "my-service"
```

## Image management

The library provides [image management](https://hackage-content.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-Images.html) for both cluster types. You can load an image onto the cluster directly from your local Docker/Podman system, or from a tarball.

An important thing to understand here is that an image name once it's loaded onto the cluster may not be the same as its local name. The [loadImage](https://hackage-content.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-Images.html#v:loadImage) function will return the cluster name of the image. This is the name you should use if you proceed to e.g. create a Pod using this image.

### Loading images

```haskell
it "loads images into cluster" $ do
  -- Load from Docker
  busyBoxImage <- loadImage (ImageLoadSpecDocker "busybox:latest" IfNotPresent)

  -- Load from Podman
  nginxImage <- loadImage (ImageLoadSpecPodman "nginx:latest" Always)

  -- Load from tarball
  tarPath <- buildNixCallPackageDerivation myImageDerivation
  tarballImage <- loadImage (ImageLoadSpecTarball tarPath)

  -- Use these images
```

### Checking loaded images

```haskell
it "checks available images" $ do
  images <- getLoadedImages
  forM_ images $ \image -> info [i|Available image: #{image}|]

  hasImage <- clusterContainsImage "busybox:latest"
  hasImage `shouldBe` True
```

## Service forwarding and port forwarding

### Service forwarding

Forward Kubernetes services to local URLs:

```haskell
it "forwards a service" $ do
  withForwardKubernetesService "default" "my-service" $ \serviceUrl -> do
    info [i|Service available at: #{serviceUrl}|]
    -- Make requests to the service
```

### Port forwarding

Forward specific pods or services to local ports:

```haskell
it "port forwards to a pod" $ do
  withKubectlPortForward "default" "pod/my-pod" "8080:80" $ \localPort -> do
    info [i|Pod forwarded to local port: #{localPort}|]
    -- Connect to localhost:localPort
```

## Namespaces

Create and destroy [Kubernetes namespaces](https://hackage-content.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-Namespace.html):

```haskell
spec :: TopSpec
spec = introduceKindClusterViaNix defaultKindClusterOptions $
  withKubernetesNamespace "my-test-namespace" $ do
    it "runs in custom namespace" $ do
      -- Tests run within the "my-test-namespace" namespace
```

## MinIO integration

The library includes built-in support for deploying [MinIO](https://www.min.io/) object storage using the [operator](https://docs.min.io/enterprise/aistor-object-store/installation/kubernetes/install/).

Once you deploy the operator, you can create a MinIO server using the [introduceK8SMinioS3Server](https://hackage-content.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes-MinioS3Server.html#v:introduceK8SMinioS3Server) family of functions.

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-kubernetes-kind/app/Main.hs"
spec :: TopSpec
spec = introduceKindClusterViaNix defaultKindClusterOptions $
  introduceBinaryViaNixPackage @"kubectl" "kubectl" $
    introduceMinioOperator defaultMinioOperatorOptions $
      withKubernetesNamespace "minio-test" $
        introduceK8SMinioS3Server (defaultMinioS3ServerOptions "minio-test") $ do
          it "has MinIO S3 server" $ do
            server <- getContext testS3Server
            info [i|Got S3 server: #{server}|]
```

## Logging

The [withKubectlLogs](https://hackage-content.haskell.org/package/sandwich-contexts-kubernetes/docs/Test-Sandwich-Contexts-Kubernetes.html#v:withKubectlLogs) function will run a `kubectl logs` process, placing the logs in a file in the current test node directory.

Note that this will stop working if the pod you're talking to goes away (even if you do it against a service). If this happens, a rerun of the command is needed to resume log forwarding.

```haskell
it "gets pod logs" $ do
  logs <- kubectlLogs "default" "my-pod" []
  info [i|Pod logs: #{logs}|]

  -- Follow logs in real-time
  withKubectlLogs "default" "my-pod" ["-f"] $ \logHandle -> do
    -- Process streaming logs
```
