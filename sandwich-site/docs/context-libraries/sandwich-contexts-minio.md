---
id: sandwich-contexts-minio
title: MinIO
---

The [sandwich-contexts-minio](https://hackage.haskell.org/package/sandwich-contexts-minio) package provides contexts for introducing [MinIO](https://min.io/) S3-compatible object storage servers in your tests. MinIO can be launched either as a raw binary or via a container system.

The MinIO server is introduced as a generic [TestS3Server](https://hackage-content.haskell.org/package/sandwich-contexts-minio/docs/Test-Sandwich-Contexts-MinIO.html#t:TestS3Server), which gives you the flexibility to easily swap out different S3-compatible storage systems in your tests.

## Starting MinIO servers

### Via Nix-provided binary

The simplest approach uses Nix to obtain the MinIO binary automatically:

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-minio/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing MinIO" $
  introduceNixContext nixpkgsReleaseDefault $
    introduceMinIOViaNix defaultMinIOContextOptions $ do
      it "prints the MinIO server info" $ do
        server <- getContext testS3Server
        info [i|Got S3 server: #{server}|]
```

### Via container

You can also run MinIO in a container using Docker or Podman:

```haskell title="https://github.com/codedownio/sandwich/blob/master/demos/demo-minio/app/Main.hs"
spec :: TopSpec
spec = describe "Introducing MinIO" $
  introduceMinIOViaContainer defaultMinIOContextOptions defaultContainerOptions $ do
    it "prints the MinIO server info" $ do
      server <- getContext testS3Server
      info [i|Got S3 server: #{server}|]
```

### Via existing binary

If you already have a MinIO binary available in your environment:

```haskell
spec :: TopSpec
spec = describe "MinIO with existing binary" $
  introduceBinaryViaEnvironment @"minio" $
    introduceMinIOViaBinary defaultMinIOContextOptions $ do
      it "uses the MinIO server" $ do
        server <- getContext testS3Server
        info [i|Got S3 server: #{server}|]
```

## Configuration options

The [MinIOContextOptions](https://hackage-content.haskell.org/package/sandwich-contexts-minio/docs/Test-Sandwich-Contexts-MinIO.html#t:MinIOContextOptions) type allows you to customize the MinIO server setup:

```haskell
data MinIOContextOptions = MinIOContextOptions {
  minioContextBucket :: Maybe Text              -- Default bucket to create
  , minioContextLabels :: Map Text Text         -- Container labels (for container mode)
  , minioContextStartupTimeout :: Int           -- Startup timeout in microseconds
}
```

**Default options:**
- **Bucket**: `"bucket1"`
- **Labels**: `mempty`
- **Startup timeout**: 60 seconds

You can customize these options:

```haskell
customOptions :: MinIOContextOptions
customOptions = defaultMinIOContextOptions {
  minioContextBucket = Just "my-test-bucket"
  , minioContextStartupTimeout = 120_000_000  -- 2 minutes
}

spec :: TopSpec
spec = introduceNixContext nixpkgsReleaseDefault $
  introduceMinIOViaNix customOptions $ do
    -- Your tests with custom configuration
```

## Working with the TestS3Server

Once you have a MinIO server running, you can access its configuration through the `TestS3Server` context:

```haskell
it "connects to MinIO server" $ do
  server <- getContext testS3Server

  -- Access server details
  info [i|MinIO endpoint: #{testS3ServerEndpoint server}|]
  info [i|Access key: #{testS3ServerAccessKeyId server}|]
  info [i|Secret key: #{testS3ServerSecretAccessKey server}|]
  info [i|Bucket: #{testS3ServerBucket server}|]
```

### Default credentials

MinIO servers started by this library use the default MinIO credentials:
- **Access Key ID**: `minioadmin`
- **Secret Access Key**: `minioadmin`

### Connection helpers

The library provides helper functions for creating MinIO connections:

```haskell
import Network.Minio

it "creates MinIO connection" $ do
  server <- getContext testS3Server

  -- Get a ConnectInfo for the minio-hs library
  let connInfo = testS3ServerConnectInfo server

  -- Use the connection
  result <- liftIO $ runMinio connInfo $ do
    buckets <- listBuckets
    return $ length buckets

  info [i|Found #{result} buckets|]
```

## Integration examples

### Working with buckets and objects

```haskell
import Network.Minio

it "performs S3 operations" $ do
  server <- getContext testS3Server
  let connInfo = testS3ServerConnectInfo server

  liftIO $ runMinio connInfo $ do
    -- List buckets
    buckets <- listBuckets
    info [i|Available buckets: #{buckets}|]

    -- Upload an object (if bucket exists)
    whenJust (testS3ServerBucket server) $ \bucket -> do
      let objectName = "test-file.txt"
      let content = "Hello, MinIO!"

      putObject bucket objectName content [] []
      info [i|Uploaded object #{objectName} to bucket #{bucket}|]

      -- Download the object back
      result <- getObject bucket objectName
      downloadedContent <- loadBytes result
      info [i|Downloaded content: #{downloadedContent}|]
```

### Using with HTTP clients

Since MinIO provides an HTTP API, you can also interact with it using standard HTTP libraries:

```haskell
import Network.HTTP.Simple

it "makes HTTP requests to MinIO" $ do
  server <- getContext testS3Server
  let endpoint = testS3ServerEndpoint server

  -- Make a request to the health endpoint
  request <- parseRequest $ toString $ endpoint <> "/minio/health/live"
  response <- httpLBS request

  getResponseStatusCode response `shouldBe` 200
  info [i|MinIO health check passed|]
```

## Container mode specifics

When using container mode, additional considerations apply:

### Container options

You can customize container behavior using `ContainerOptions`:

```haskell
customContainerOptions :: ContainerOptions
customContainerOptions = defaultContainerOptions {
  containerOptionsSystem = ContainerSystemDocker
  , containerOptionsName = Just "my-minio-server"
}

spec :: TopSpec
spec = introduceMinIOViaContainer defaultMinIOContextOptions customContainerOptions $ do
  -- Your tests here
```

### Volume mounting

The container mode automatically creates and mounts a temporary directory for MinIO data storage. This directory is cleaned up when the container is destroyed.

### Port mapping

The library automatically handles port mapping, finding an available local port and mapping it to MinIO's internal port (9000). The [testS3ServerEndpoint](https://hackage-content.haskell.org/package/sandwich-contexts-minio/docs/Test-Sandwich-Contexts-MinIO.html#v:testS3ServerEndpoint) function will give you the correct local address to connect to.

## Bracket-style functions

For more control over server lifecycle, you can use the bracket-style functions:

```haskell
import Control.Monad.IO.Unlift

it "uses bracket-style MinIO" $ do
  withMinIOViaBinary "/path/to/minio" defaultMinIOContextOptions $ \server -> do
    -- Server is running here
    let endpoint = testS3ServerEndpoint server
    info [i|Server available at: #{endpoint}|]

    -- Your test logic here

    -- Server will be automatically cleaned up
```

These functions are useful when you need to start multiple MinIO servers or when integrating with other resource management patterns.
