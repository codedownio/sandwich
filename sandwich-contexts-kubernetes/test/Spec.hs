{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import qualified Data.List as L
import Data.String.Interpolate
import Relude
import System.Exit
import qualified System.Random as R
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes.Images
import Test.Sandwich.Contexts.Kubernetes.KindCluster
import Test.Sandwich.Contexts.Kubernetes.Kubectl
import Test.Sandwich.Contexts.Kubernetes.MinikubeCluster
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
import UnliftIO.Exception
import UnliftIO.Process


spec :: TopSpec
spec = introduceNixContext nixpkgsReleaseDefault $
  introduceBinaryViaNixPackage @"kubectl" "kubectl" $ do
    describe "Minikube" $ introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $
      loadImageTests

    describe "Kind" $ introduceKindClusterViaNix defaultKindClusterOptions $
      loadImageTests

imageLoadSpecs :: [ImageLoadSpec]
imageLoadSpecs = [
  ImageLoadSpecDocker "busybox:latest" IfNotPresent
  , ImageLoadSpecDocker "registry.k8s.io/pause:3.9" IfNotPresent
  , ImageLoadSpecDocker "gcr.io/distroless/static-debian11:latest" IfNotPresent
  ]

tarballDerivations :: [(Text, Text)]
tarballDerivations = [
  ("busybox-tarball", busyboxDerivation)
  ]

imageLabel :: Label "image" Text
imageLabel = Label

imageLoadSpecLabel :: Label "imageLoadSpec" ImageLoadSpec
imageLoadSpecLabel = Label

opts :: NodeOptions
opts = defaultNodeOptions { nodeOptionsVisibilityThreshold = 50 }

loadImageTests :: (
  MonadUnliftIO m, MonadMask m
  , HasBaseContext context, HasKubernetesClusterContext context, HasNixContext context, HasFile context "kubectl"
  ) => SpecFree context m ()
loadImageTests = do
  it "prints the cluster info" $ do
    kcc <- getContext kubernetesCluster
    info [i|Got Kubernetes cluster context: #{kcc}|]

  forM_ tarballDerivations $ \(name, derivation) ->
    introduce' opts [i|#{name}|] imageLoadSpecLabel (ImageLoadSpecTarball <$> buildNixCallPackageDerivation derivation) (const $ return ()) $
    introduce [i|#{name} load (tarball)|] imageLabel (getContext imageLoadSpecLabel >>= loadImage) (const $ return ()) $ do
      loadImageTests'

  forM_ imageLoadSpecs $ \ils ->
    introduce' opts [i|#{ils}|] imageLoadSpecLabel (pure ils) (const $ return ()) $
    introduce [i|#{ils} load|] imageLabel (loadImage ils) (const $ return ()) $ do
      loadImageTests'

loadImageTests' :: (
  MonadUnliftIO m
  , HasBaseContext context
  , HasKubernetesClusterContext context
  , HasFile context "kubectl"
  , HasLabel context "image" Text, HasLabel context "imageLoadSpec" ImageLoadSpec
  ) => SpecFree context m ()
loadImageTests' = do
  it "Doesn't transform Docker/Podman image names" $ do
    image <- getContext imageLabel
    getContext imageLoadSpecLabel >>= \case
      ImageLoadSpecTarball {} -> return ()
      ImageLoadSpecDocker initialImage _ -> image `shouldBe` initialImage
      ImageLoadSpecPodman initialImage _ -> image `shouldBe` initialImage

  it "Cluster contains the image" $ do
    images <- getLoadedImages
    forM_ images $ \img ->
      info [i|loaded image: #{img}|]

    image <- getContext imageLabel
    clusterContainsImage image >>= \case
      False -> expectationFailure [i|Cluster didn't contain image '#{image}'|]
      True -> return ()

  it "Creates a pod and the cluster finds the image already present" $ do
    image <- getContext imageLabel
    podName <- ("test-pod-" <>) <$> randomAlpha 8

    -- namespace <- ("test-namespace-" <>) <$> randomAlpha 8
    -- withKubernetesNamespace' (toText namespace) $
    let namespace = "default"

    (kubectlBinary, env) <- runWithKubectl

    -- Wait for service account to exist; see
    -- https://github.com/kubernetes/kubernetes/issues/66689
    waitUntil 60 $
      createProcessWithLogging ((proc kubectlBinary ["--namespace", namespace
                                                    , "get", "serviceaccount", "default"
                                                    , "-o", "name"]) { env = Just env })
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    let deletePod = createProcessWithLogging ((proc kubectlBinary ["--namespace", namespace
                                                                  , "delete", "pod", podName]) { env = Just env })
                      >>= waitForProcess >>= (`shouldBe` ExitSuccess)

    flip finally deletePod $ do
      createProcessWithLogging ((proc kubectlBinary ["--namespace", namespace
                                                    , "run", podName
                                                    , "--image", toString image
                                                    , "--image-pull-policy=IfNotPresent"
                                                    , "--command", "--", "/bin/sh", "-c", "sleep infinity"
                                                    ]) { env = Just env })
        >>= waitForProcess >>= (`shouldBe` ExitSuccess)

      waitUntil 300 $ do
        events <- readCreateProcessWithLogging ((proc kubectlBinary ["--namespace", namespace
                                                                    , "get", "events"
                                                                    , "--field-selector", [i|involvedObject.kind=Pod,involvedObject.name=#{podName}|]
                                                                    ]) { env = Just env }) ""
        info [i|events: #{events}|]
        events `shouldContain` "already present on machine"



randomAlpha :: MonadIO m => Int -> m String
randomAlpha len = liftIO $ do
  gen <- R.newStdGen
  return $ L.take len (R.randomRs ('a', 'z') gen)

busyboxDerivation :: Text
busyboxDerivation = [i|
{ dockerTools }:

dockerTools.pullImage {
  imageName = "busybox";
  imageDigest = "sha256:9ae97d36d26566ff84e8893c64a6dc4fe8ca6d1144bf5b87b2b85a32def253c7";
  sha256 = "sha256-S4jXnRLZMZUyxjPku3jczd2PwCsFKR4TXRcIy3C/ym8=";
  finalImageName = "busybox-tarball";
  finalImageTag = "latest";
}
|]

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
