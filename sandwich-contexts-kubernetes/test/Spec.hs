{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Nix
import Test.Sandwich.Contexts.Waits
import UnliftIO.Process
-- import Test.Sandwich.Contexts.Waits


spec :: TopSpec
spec = introduceNixContext nixpkgsReleaseDefault $
  introduceBinaryViaNixPackage @"kubectl" "kubectl" $ do
    describe "Minikube" $ introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $
      loadImageTests

    describe "Kind" $ introduceKindClusterViaNix defaultKindClusterOptions $
      loadImageTests

loadImageTests :: (
  MonadUnliftIO m
  , HasBaseContext context, HasKubernetesClusterContext context, HasFile context "kubectl"
  ) => SpecFree context m ()
loadImageTests = do
  it "prints the cluster info" $ do
    kcc <- getContext kubernetesCluster
    info [i|Got Kubernetes cluster context: #{kcc}|]

  forM_ ["busybox:latest", "registry.k8s.io/pause:3.9", "gcr.io/distroless/static-debian11:latest"] $ \image ->
    it [i|#{image}|] $ do
      -- dockerPullIfNecessary image

      transformedImageName <- loadImage (ImageLoadSpecDockerImage image IfNotPresent)

      transformedImageName `shouldBe` image

      images <- getLoadedImages
      forM_ images $ \img ->
        info [i|loaded image: #{img}|]

      clusterContainsImage transformedImageName >>= \case
        False -> expectationFailure [i|Cluster didn't contain image '#{transformedImageName}'|]
        True -> return ()

      namespace <- ("test-namespace-" <>) <$> randomAlpha 8
      podName <- ("test-pod-" <>) <$> randomAlpha 8
      withKubernetesNamespace' (toText namespace) $
        runWithKubectl $ \kubectlBinary env -> do
          -- Wait for service account to exist; see
          -- https://github.com/kubernetes/kubernetes/issues/66689
          waitUntil 60 $
            createProcessWithLogging ((proc kubectlBinary ["--namespace", namespace, "get", "serviceaccount", "default", "-o", "name"]) { env = Just env })
              >>= waitForProcess >>= (`shouldBe` ExitSuccess)

          createProcessWithLogging ((proc kubectlBinary ["--namespace", namespace
                                                        , "run", podName
                                                        , "--image", toString transformedImageName
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


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
