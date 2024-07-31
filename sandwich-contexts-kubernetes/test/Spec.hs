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

imageLabel :: Label "image" Text
imageLabel = Label

loadImageTests :: (
  MonadUnliftIO m
  , HasBaseContext context, HasKubernetesClusterContext context, HasFile context "kubectl"
  ) => SpecFree context m ()
loadImageTests = do
  it "prints the cluster info" $ do
    kcc <- getContext kubernetesCluster
    info [i|Got Kubernetes cluster context: #{kcc}|]

  forM_ imageLoadSpecs $ \ils ->
    introduce [i|#{ils} load|] imageLabel (loadImage ils) (const $ return ()) $ do
      it "Doesn't transform Docker/Podman image names" $ do
        image <- getContext imageLabel
        case ils of
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

        runWithKubectl $ \kubectlBinary env -> do
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


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
