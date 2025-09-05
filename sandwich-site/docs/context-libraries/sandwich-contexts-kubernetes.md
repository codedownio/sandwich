---
id: sandwich-contexts-kubernetes
title: Kubernetes
---

The [sandwich-contexts-kubernetes](https://hackage.haskell.org/package/sandwich-contexts-kubernetes) package provides contexts for creating and managing Kubernetes clusters in your tests, along with utilities for interacting with them.

This package supports creating clusters using either [kind](https://kind.sigs.k8s.io/) or [Minikube](https://minikube.sigs.k8s.io), with binaries obtained either from the system PATH or from Nix. It also includes functions for waiting for pods and services, running kubectl commands, service forwarding, port forwarding, and image management.

## Creating clusters

### Kind clusters

[Kind](https://kind.sigs.k8s.io/) is a tool for running local Kubernetes clusters using Docker container "nodes". It's lightweight and perfect for testing.

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

### Cluster configuration

Both cluster types support various configuration options:

**Kind cluster options:**
- **Cluster name**: Set via `kindClusterOptionsClusterName`
- **Node image**: Configure Kubernetes version via `kindClusterOptionsNodeImage`
- **Port mappings**: Expose cluster ports to the host via `kindClusterOptionsExtraPortMappings`
- **Mounts**: Mount host directories into cluster nodes via `kindClusterOptionsExtraMounts`
- **Network configuration**: Custom networking via `kindClusterOptionsNetwork`

**Minikube cluster options:**
- **Profile name**: Set via `minikubeClusterOptionsProfileName`
- **Driver**: Choose container runtime via `minikubeClusterOptionsDriver` (docker, podman, etc.)
- **Flags**: Pass additional minikube flags via `minikubeClusterOptionsExtraFlags`

## Working with clusters

### Accessing cluster context

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

The library provides comprehensive image management for both cluster types:

### Loading images

```haskell
it "loads images into cluster" $ do
  -- Load from Docker
  loadImage (ImageLoadSpecDocker "busybox:latest" IfNotPresent)
  
  -- Load from Podman
  loadImage (ImageLoadSpecPodman "nginx:latest" Always)
  
  -- Load from tarball
  tarPath <- buildNixCallPackageDerivation myImageDerivation
  loadImage (ImageLoadSpecTarball tarPath)
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

## Advanced features

### Namespaces

Create and work within Kubernetes namespaces:

```haskell
spec :: TopSpec
spec = introduceKindClusterViaNix defaultKindClusterOptions $
  withKubernetesNamespace "my-test-namespace" $ do
    it "runs in custom namespace" $ do
      -- Tests run within the "my-test-namespace" namespace
```

### MinIO integration

The library includes built-in support for MinIO object storage:

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

### Logging

Capture and analyze pod logs:

```haskell
it "gets pod logs" $ do
  logs <- kubectlLogs "default" "my-pod" []
  info [i|Pod logs: #{logs}|]
  
  -- Follow logs in real-time
  withKubectlLogs "default" "my-pod" ["-f"] $ \logHandle -> do
    -- Process streaming logs
```

## Type constraints

The library provides several constraint aliases to make function signatures cleaner:

- `KubernetesBasic context m`: Basic Kubernetes functionality
- `KubernetesClusterBasic context m`: Requires a cluster context
- `KubectlBasic context m`: Requires kubectl binary and cluster context
- `HasKubernetesClusterContext context`: Context has cluster information

These constraints help ensure your functions have access to the required contexts and capabilities.
