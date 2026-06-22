{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- | Demo bringing up SeaweedFS on a Minikube cluster and exercising the CSI driver:
-- one pod mounts a SeaweedFS-backed PersistentVolume and writes a file, then a second
-- pod mounts the same volume and reads the file back.

module Main where

import Data.String.Interpolate
import qualified Data.Text as T
import Relude
import System.Exit (ExitCode(..))
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Kubernetes
import Test.Sandwich.Contexts.Kubernetes.Namespace
import Test.Sandwich.Contexts.Kubernetes.SeaweedFS
import Test.Sandwich.Contexts.Nix
import UnliftIO.Process


demoNamespace :: Text
demoNamespace = "foo"

-- | A unique marker we write from the first pod and look for from the second.
magicString :: Text
magicString = "hello-from-seaweedfs-csi-pvc"

-- | Name of the shared PVC the two pods mount.
pvcName :: Text
pvcName = "seaweed-shared"

spec :: TopSpec
spec = describe "Introducing a Kubernetes cluster" $ do
  describe "Via Minikube" $
    introduceNixContext nixpkgsReleaseDefault $
    introduceBinaryViaNixPackage @"kubectl" "kubectl" $
    introduceMinikubeClusterViaNix defaultMinikubeClusterOptions $ do
      it "prints the cluster info" $ do
        kcc <- getContext kubernetesCluster
        info [i|export KUBECONFIG='#{kubernetesClusterKubeConfigPath kcc}'|]
        info [i|Got Kubernetes cluster context: #{kcc}|]

      -- introduceSeaweedFS installs the CSI driver by default (seaweedFsCsiDriver), so
      -- a "seaweedfs-storage" StorageClass is available to back PersistentVolumes.
      withKubernetesNamespace demoNamespace $ introduceSeaweedFS demoNamespace fastSeaweedFSOptions $ do
        it "Has a SeaweedFS context" $ do
          sfs <- getContext seaweedFs
          info [i|Got SeaweedFS context: #{sfs}|]

        before "Provision a SeaweedFS-backed PVC" createSharedPvc $ do
          it "writes a file to the volume from one pod" $ do
            applyYaml "seaweed-writer-job" $
              jobYaml "seaweed-writer" [i|echo '#{magicString}' > /data/hello.txt && sync && ls -la /data/hello.txt|]
            waitForJobComplete "seaweed-writer"
            info [i|Wrote '#{magicString}' to /data/hello.txt on the SeaweedFS volume|]

          it "reads the file back from a second pod" $ do
            applyYaml "seaweed-reader-job" $
              jobYaml "seaweed-reader" [i|cat /data/hello.txt|]
            waitForJobComplete "seaweed-reader"
            output <- toText <$> kubectlCapture ["logs", "-n", toString demoNamespace, "job/seaweed-reader"]
            info [i|Reader pod output: #{output}|]
            unless (magicString `T.isInfixOf` output) $
              expectationFailure [i|Expected to read '#{magicString}' back from the SeaweedFS volume, but got: #{output}|]


-- | Create the ReadWriteMany PVC the two pods share, on the StorageClass the SeaweedFS
-- CSI driver installed (taken from the context).
createSharedPvc :: (KubectlBasic context m, HasSeaweedFSContext context, MonadFail m) => m ()
createSharedPvc = do
  sfs <- getContext seaweedFs
  storageClass <- case seaweedFsCsiStorageClass sfs of
    Just sc -> pure sc
    Nothing -> expectationFailure [i|SeaweedFS CSI driver wasn't installed (seaweedFsCsiDriver = Nothing); can't provision a PVC|]
  info [i|Creating ReadWriteMany PVC '#{pvcName}' on StorageClass '#{storageClass}'|]
  applyYaml "seaweed-pvc" (pvcYaml storageClass)


-- | Apply a manifest by piping it to @kubectl apply -f -@.
applyYaml :: (KubectlBasic context m, MonadFail m) => String -> Text -> m ()
applyYaml name yaml = do
  (kubectl, env) <- askKubectlArgs
  createProcessWithFileLoggingAndStdin' name ((proc kubectl ["apply", "-f", "-"]) { env = Just env }) (toString yaml)
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

-- | Wait for a Job in the demo namespace to complete.
waitForJobComplete :: (KubectlBasic context m) => Text -> m ()
waitForJobComplete name = do
  (kubectl, env) <- askKubectlArgs
  createProcessWithFileLogging' [i|wait-#{name}|]
    ((proc kubectl ["wait", "-n", toString demoNamespace, "--for=condition=complete", "--timeout=180s", [i|job/#{name}|]]) { env = Just env })
    >>= waitForProcess >>= (`shouldBe` ExitSuccess)

-- | Run a @kubectl@ command and capture its stdout.
kubectlCapture :: (KubectlBasic context m) => [String] -> m String
kubectlCapture args = do
  (kubectl, env) <- askKubectlArgs
  readCreateProcess ((proc kubectl args) { env = Just env }) ""


pvcYaml :: Text -> Text
pvcYaml storageClass = [i|apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: #{pvcName}
  namespace: #{demoNamespace}
spec:
  accessModes: ["ReadWriteMany"]
  storageClassName: #{storageClass}
  resources:
    requests:
      storage: 1Gi
|]

-- | A run-to-completion Job that mounts the shared PVC at @/data@ and runs @cmd@.
jobYaml :: Text -> Text -> Text
jobYaml name cmd = [i|apiVersion: batch/v1
kind: Job
metadata:
  name: #{name}
  namespace: #{demoNamespace}
spec:
  backoffLimit: 0
  template:
    spec:
      restartPolicy: Never
      containers:
      - name: app
        image: busybox:1.36
        command: ["sh", "-c", "#{cmd}"]
        volumeMounts:
        - name: data
          mountPath: /data
      volumes:
      - name: data
        persistentVolumeClaim:
          claimName: #{pvcName}
|]


main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
